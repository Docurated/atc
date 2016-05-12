package worker

import (
	"errors"
	"time"

	gclient "github.com/cloudfoundry-incubator/garden/client"
	gconn "github.com/cloudfoundry-incubator/garden/client/connection"
	"github.com/cloudfoundry-incubator/garden/routes"
	"github.com/concourse/baggageclaim"
	bclient "github.com/concourse/baggageclaim/client"
	"github.com/pivotal-golang/clock"
	"github.com/pivotal-golang/lager"
	"github.com/tedsuo/rata"

	"github.com/concourse/atc/db"
)

//go:generate counterfeiter . WorkerDB

type WorkerDB interface {
	Workers() ([]db.SavedWorker, error)
	GetWorker(string) (db.SavedWorker, bool, error)
	CreateContainer(db.Container, time.Duration, time.Duration) (db.SavedContainer, error)
	GetContainer(string) (db.SavedContainer, bool, error)
	FindContainerByIdentifier(db.ContainerIdentifier) (db.SavedContainer, bool, error)

	UpdateExpiresAtOnContainer(handle string, ttl time.Duration) error
	ReapContainer(handle string) error

	InsertVolume(db.Volume) error
	GetVolumesByIdentifier(db.VolumeIdentifier) ([]db.SavedVolume, error)
	GetVolumeTTL(volumeHandle string) (time.Duration, bool, error)
	ReapVolume(handle string) error
	SetVolumeTTL(string, time.Duration) error
	SetVolumeSize(string, uint) error
}

var ErrMultipleWorkersWithName = errors.New("More than one worker has given worker name")

type dbProvider struct {
	logger       lager.Logger
	db           WorkerDB
	dialer       gconn.DialerFunc
	retryPolicy  RetryPolicy
	imageFetcher ImageFetcher
}

func NewDBWorkerProvider(
	logger lager.Logger,
	db WorkerDB,
	dialer gconn.DialerFunc,
	retryPolicy RetryPolicy,
	imageFetcher ImageFetcher,
) WorkerProvider {
	return &dbProvider{
		logger:       logger,
		db:           db,
		dialer:       dialer,
		retryPolicy:  retryPolicy,
		imageFetcher: imageFetcher,
	}
}

func (provider *dbProvider) Workers() ([]Worker, error) {
	savedWorkers, err := provider.db.Workers()
	if err != nil {
		return nil, err
	}

	tikTok := clock.NewClock()

	workers := make([]Worker, len(savedWorkers))

	for i, savedWorker := range savedWorkers {
		workers[i] = provider.newGardenWorker(tikTok, savedWorker)
	}

	return workers, nil
}

func (provider *dbProvider) GetWorker(name string) (Worker, bool, error) {
	savedWorker, found, err := provider.db.GetWorker(name)
	if err != nil {
		return nil, false, err
	}

	if !found {
		return nil, false, nil
	}

	tikTok := clock.NewClock()

	worker := provider.newGardenWorker(tikTok, savedWorker)

	return worker, found, nil
}

func (provider *dbProvider) FindContainerForIdentifier(id Identifier) (db.SavedContainer, bool, error) {
	return provider.db.FindContainerByIdentifier(db.ContainerIdentifier(id))
}

func (provider *dbProvider) GetContainer(handle string) (db.SavedContainer, bool, error) {
	return provider.db.GetContainer(handle)
}

func (provider *dbProvider) ReapContainer(handle string) error {
	return provider.db.ReapContainer(handle)
}

func (provider *dbProvider) newGardenWorker(tikTok clock.Clock, savedWorker db.SavedWorker) Worker {
	workerLog := provider.logger.Session("worker-connection", lager.Data{
		"addr": savedWorker.GardenAddr,
	})

	gcf := NewGardenConnectionFactory(
		provider.db,
		provider.logger.Session("garden-connection"),
		savedWorker.Name,
		savedWorker.GardenAddr,
	)

	gardenConn := NewRetryableConnection(
		workerLog,
		// TODO: why do we need two declarations of this struct?
		WorkerHijackStreamer{
			HijackStreamer: gconn.NewHijackStreamer("tcp", savedWorker.GardenAddr),
			HttpClient:     gcf.CreateRetryableHttpClient(),
			req:            rata.NewRequestGenerator("http://"+savedWorker.GardenAddr, routes.Routes),
		},
		gcf,
	)

	var bClient baggageclaim.Client
	if savedWorker.BaggageclaimURL != "" {
		bClient = bclient.New(savedWorker.BaggageclaimURL)
	}

	volumeFactory := NewVolumeFactory(
		provider.db,
		tikTok,
	)

	volumeClient := NewVolumeClient(
		bClient,
		provider.db,
		volumeFactory,
		savedWorker.Name,
	)

	return NewGardenWorker(
		gclient.New(gardenConn),
		bClient,
		volumeClient,
		volumeFactory,
		provider.imageFetcher,
		provider.db,
		provider,
		tikTok,
		savedWorker.ActiveContainers,
		savedWorker.ResourceTypes,
		savedWorker.Platform,
		savedWorker.Tags,
		savedWorker.Name,
		savedWorker.StartTime,
		savedWorker.HTTPProxyURL,
		savedWorker.HTTPSProxyURL,
		savedWorker.NoProxy,
	)
}
