package db_test

import (
	"time"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
	"github.com/lib/pq"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Image Versions", func() {
	var dbConn db.Conn
	var listener *pq.Listener

	var pipelineDBFactory db.PipelineDBFactory
	var sqlDB *db.SQLDB
	var pipelineDB db.PipelineDB

	BeforeEach(func() {
		postgresRunner.Truncate()

		dbConn = db.Wrap(postgresRunner.Open())

		listener = pq.NewListener(postgresRunner.DataSourceName(), time.Second, time.Minute, nil)
		Eventually(listener.Ping, 5*time.Second).ShouldNot(HaveOccurred())
		bus := db.NewNotificationsBus(listener, dbConn)

		sqlDB = db.NewSQL(dbConn, bus)
		pipelineDBFactory = db.NewPipelineDBFactory(dbConn, bus)

		_, err := sqlDB.CreateTeam(db.Team{Name: "some-team"})
		Expect(err).NotTo(HaveOccurred())
		teamDBFactory := db.NewTeamDBFactory(dbConn)
		teamDB := teamDBFactory.GetTeamDB("some-team")

		config := atc.Config{
			Jobs: atc.JobConfigs{
				{
					Name: "some-job",
				},
			},
		}

		savedPipeline, _, err := teamDB.SaveConfig("a-pipeline-name", config, 0, db.PipelineUnpaused)
		Expect(err).NotTo(HaveOccurred())

		pipelineDB = pipelineDBFactory.Build(savedPipeline)
	})

	AfterEach(func() {
		err := dbConn.Close()
		Expect(err).NotTo(HaveOccurred())

		err = listener.Close()
		Expect(err).NotTo(HaveOccurred())
	})

	It("can retrieve saved image_resource_versions from the database", func() {
		build, err := pipelineDB.CreateJobBuild("some-job")
		Expect(err).ToNot(HaveOccurred())

		otherBuild, err := pipelineDB.CreateJobBuild("some-job")
		Expect(err).ToNot(HaveOccurred())

		identifier := db.ResourceCacheIdentifier{
			ResourceVersion: atc.Version{"ref": "our super sweet ref"},
			ResourceHash:    "our even sweeter resource hash",
		}

		otherIdentifier := db.ResourceCacheIdentifier{
			ResourceVersion: atc.Version{"ref": "our super sweet ref"},
			ResourceHash:    "our even sweeter resource hash",
		}

		badIdentifier := db.ResourceCacheIdentifier{
			ResourceVersion: atc.Version{"ref": "our super bad ref"},
			ResourceHash:    "our even badder resource hash",
		}

		err = sqlDB.SaveImageResourceVersion(build.ID, "our-super-sweet-plan", identifier)
		Expect(err).ToNot(HaveOccurred())

		err = sqlDB.SaveImageResourceVersion(build.ID, "our-other-super-sweet-plan", otherIdentifier)
		Expect(err).ToNot(HaveOccurred())

		err = sqlDB.SaveImageResourceVersion(otherBuild.ID, "our-super-bad-plan", badIdentifier)
		Expect(err).ToNot(HaveOccurred())

		recoveredIdentifiers, err := sqlDB.GetImageResourceCacheIdentifiersByBuildID(build.ID)
		Expect(err).ToNot(HaveOccurred())

		Expect(recoveredIdentifiers).To(ConsistOf(identifier, otherIdentifier))

		By("not saving versions for non-existent build ids")

		err = sqlDB.SaveImageResourceVersion(555, "our-super-fake-plan", badIdentifier)
		Expect(err).To(HaveOccurred())

		recoveredFakeIdentifiers, err := sqlDB.GetImageResourceCacheIdentifiersByBuildID(555)
		Expect(err).NotTo(HaveOccurred())
		Expect(recoveredFakeIdentifiers).To(BeEmpty())

		By("replacing the version if the id combination already exists")

		err = sqlDB.SaveImageResourceVersion(build.ID, "our-super-sweet-plan", badIdentifier)
		Expect(err).ToNot(HaveOccurred())

		recoveredIdentifiers, err = sqlDB.GetImageResourceCacheIdentifiersByBuildID(build.ID)
		Expect(err).ToNot(HaveOccurred())

		Expect(recoveredIdentifiers).To(ConsistOf(badIdentifier, otherIdentifier))

		By("not not enforcing global uniqueness of plan IDs")

		err = sqlDB.SaveImageResourceVersion(otherBuild.ID, "our-super-sweet-plan", badIdentifier)
		Expect(err).ToNot(HaveOccurred())

		otherRecoveredIdentifiers, err := sqlDB.GetImageResourceCacheIdentifiersByBuildID(otherBuild.ID)
		Expect(err).ToNot(HaveOccurred())

		Expect(otherRecoveredIdentifiers).To(ConsistOf(badIdentifier, badIdentifier))
	})
})
