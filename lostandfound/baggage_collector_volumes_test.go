package lostandfound_test

import (
	"errors"
	"time"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
	"github.com/concourse/atc/lostandfound"
	"github.com/concourse/atc/lostandfound/fakes"
	"github.com/concourse/atc/resource"
	"github.com/pivotal-golang/lager/lagertest"

	dbfakes "github.com/concourse/atc/db/fakes"
	wfakes "github.com/concourse/atc/worker/fakes"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Volumes are reaped", func() {
	var (
		fakeWorkerClient *wfakes.FakeClient
		fakeWorker       *wfakes.FakeWorker

		fakePipelineDBFactory          *dbfakes.FakePipelineDBFactory
		fakeBaggageCollectorDB         *fakes.FakeBaggageCollectorDB
		expectedOldResourceGracePeriod = 4 * time.Minute
		expectedOneOffTTL              = 5 * time.Hour

		baggageCollector          lostandfound.BaggageCollector
		returnedSavedVolume       db.SavedVolume
		newestReturnedSavedVolume db.SavedVolume
		returnedVolumes           []db.SavedVolume
	)

	BeforeEach(func() {
		fakeWorkerClient = new(wfakes.FakeClient)
		fakeWorker = new(wfakes.FakeWorker)
		baggageCollectorLogger := lagertest.NewTestLogger("test")
		fakeBaggageCollectorDB = new(fakes.FakeBaggageCollectorDB)
		fakePipelineDBFactory = new(dbfakes.FakePipelineDBFactory)

		baggageCollector = lostandfound.NewBaggageCollector(
			baggageCollectorLogger,
			fakeWorkerClient,
			fakeBaggageCollectorDB,
			fakePipelineDBFactory,
			expectedOldResourceGracePeriod,
			expectedOneOffTTL,
		)

		returnedSavedVolume = db.SavedVolume{
			Volume: db.Volume{
				WorkerName: "a-new-worker",
				TTL:        time.Minute,
				Handle:     "some-handle",
				Identifier: db.VolumeIdentifier{
					ResourceCache: &db.ResourceCacheIdentifier{
						ResourceVersion: atc.Version{"some": "version"},
						ResourceHash:    "some-hash",
					},
				},
			},
			ID:        123,
			ExpiresIn: expectedOldResourceGracePeriod,
		}

		returnedVolumes = []db.SavedVolume{returnedSavedVolume}
	})

	JustBeforeEach(func() {
		fakeBaggageCollectorDB.GetVolumesReturns(returnedVolumes, nil)
	})

	Context("when the worker for a newest resource no longer exists", func() {
		var (
			fakeSavedPipeline          db.SavedPipeline
			fakePipelineDB             dbfakes.FakePipelineDB
			fakeSavedVersionedResource db.SavedVersionedResource
		)

		BeforeEach(func() {
			fakeSavedPipeline = db.SavedPipeline{
				Pipeline: db.Pipeline{
					Name: "some-pipeline",
					Config: atc.Config{
						Resources: atc.ResourceConfigs{
							atc.ResourceConfig{
								Name:   "our-resource",
								Type:   "git",
								Source: atc.Source{"some": "source"},
							},
						},
					},
					Version: 42,
				},
				ID:     7,
				Paused: false,
				TeamID: 13,
			}

			fakeSavedVersionedResource = db.SavedVersionedResource{
				ID:           123,
				Enabled:      true,
				ModifiedTime: time.Now(),
				VersionedResource: db.VersionedResource{
					Resource:   "our-resource",
					Type:       "git",
					Version:    db.Version{"some": "newest-version"},
					PipelineID: fakeSavedPipeline.ID,
				},
			}

			hashkey := resource.GenerateResourceHash(
				fakeSavedPipeline.Config.Resources[0].Source,
				fakeSavedPipeline.Config.Resources[0].Type,
			)
			newestReturnedSavedVolume = db.SavedVolume{
				Volume: db.Volume{
					WorkerName: "a-new-worker",
					TTL:        0,
					Handle:     "some-other-handle",
					Identifier: db.VolumeIdentifier{
						ResourceCache: &db.ResourceCacheIdentifier{
							ResourceVersion: atc.Version{"some": "newest-version"},
							ResourceHash:    hashkey,
						},
					},
				},
				ID:        124,
				ExpiresIn: 0,
			}

			returnedVolumes = append(returnedVolumes, newestReturnedSavedVolume)

			fakeBaggageCollectorDB.GetAllPipelinesReturns([]db.SavedPipeline{fakeSavedPipeline}, nil)
			fakePipelineDBFactory.BuildReturns(&fakePipelineDB)
			fakePipelineDB.GetLatestEnabledVersionedResourceReturns(fakeSavedVersionedResource, true, nil)

			fakeWorkerClient.GetWorkerReturns(nil, errors.New("no-worker-found"))
		})

		It("should remove the volume from the database", func() {
			err := baggageCollector.Run()
			Expect(err).NotTo(HaveOccurred())

			Expect(fakeBaggageCollectorDB.ReapVolumeCallCount()).To(Equal(2))
			Expect(fakeBaggageCollectorDB.ReapVolumeArgsForCall(0)).To(Equal(returnedSavedVolume.Handle))
			Expect(fakeBaggageCollectorDB.ReapVolumeArgsForCall(1)).To(Equal(newestReturnedSavedVolume.Handle))
		})
	})

	Context("when volume no longer exists", func() {
		Context("when the worker is no longer around", func() {
			BeforeEach(func() {
				fakeWorkerClient.GetWorkerReturns(nil, errors.New("no-worker-found"))
			})

			It("removes the volume from the database", func() {
				err := baggageCollector.Run()
				Expect(err).NotTo(HaveOccurred())

				Expect(fakeBaggageCollectorDB.ReapVolumeCallCount()).To(Equal(1))
				Expect(fakeBaggageCollectorDB.ReapVolumeArgsForCall(0)).To(Equal(returnedSavedVolume.Handle))
			})
		})

		Context("the volume is no longer found on the worker", func() {
			BeforeEach(func() {
				fakeWorkerClient.GetWorkerReturns(fakeWorker, nil)
				fakeWorker.LookupVolumeReturns(nil, false, nil)
			})

			It("removes the volume from the database", func() {
				err := baggageCollector.Run()
				Expect(err).NotTo(HaveOccurred())

				Expect(fakeBaggageCollectorDB.ReapVolumeCallCount()).To(Equal(1))
				Expect(fakeBaggageCollectorDB.ReapVolumeArgsForCall(0)).To(Equal(returnedSavedVolume.Handle))
			})
		})
	})
})
