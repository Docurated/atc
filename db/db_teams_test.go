package db_test

import (
	"database/sql"
	"fmt"
	"time"

	"golang.org/x/crypto/bcrypt"

	"github.com/lib/pq"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
)

var _ = Describe("SQL DB Teams", func() {
	var dbConn db.Conn
	var listener *pq.Listener

	var database db.DB
	var teamDBFactory db.TeamDBFactory

	BeforeEach(func() {
		postgresRunner.Truncate()

		dbConn = db.Wrap(postgresRunner.Open())
		listener = pq.NewListener(postgresRunner.DataSourceName(), time.Second, time.Minute, nil)

		Eventually(listener.Ping, 5*time.Second).ShouldNot(HaveOccurred())
		bus := db.NewNotificationsBus(listener, dbConn)

		teamDBFactory = db.NewTeamDBFactory(dbConn)
		database = db.NewSQL(dbConn, bus)

		database.DeleteTeamByName(atc.DefaultTeamName)
	})

	AfterEach(func() {
		err := dbConn.Close()
		Expect(err).NotTo(HaveOccurred())

		err = listener.Close()
		Expect(err).NotTo(HaveOccurred())
	})

	Describe("CreateDefaultTeamIfNotExists", func() {
		It("creates the default team", func() {
			err := database.CreateDefaultTeamIfNotExists()
			Expect(err).NotTo(HaveOccurred())

			var count sql.NullInt64
			dbConn.QueryRow(fmt.Sprintf(`select count(1) from teams where name = '%s'`, atc.DefaultTeamName)).Scan(&count)

			Expect(count.Valid).To(BeTrue())
			Expect(count.Int64).To(Equal(int64(1)))

			team, _, err := teamDBFactory.GetTeamDB(atc.DefaultTeamName).GetTeam()
			Expect(err).NotTo(HaveOccurred())
			Expect(team.Admin).To(BeTrue())
		})

		Context("when the default team already exists", func() {
			BeforeEach(func() {
				defaultTeam := db.Team{
					Name: atc.DefaultTeamName,
				}
				_, err := database.CreateTeam(defaultTeam)
				Expect(err).NotTo(HaveOccurred())
			})

			It("does not duplicate the default team", func() {
				err := database.CreateDefaultTeamIfNotExists()
				Expect(err).NotTo(HaveOccurred())

				var count sql.NullInt64
				dbConn.QueryRow(fmt.Sprintf(`select count(1) from teams where name = '%s'`, atc.DefaultTeamName)).Scan(&count)

				Expect(count.Valid).To(BeTrue())
				Expect(count.Int64).To(Equal(int64(1)))
			})

			It("sets admin permissions on that team", func() {
				err := database.CreateDefaultTeamIfNotExists()
				Expect(err).NotTo(HaveOccurred())

				var admin bool
				dbConn.QueryRow(fmt.Sprintf(`select admin from teams where name = '%s'`, atc.DefaultTeamName)).Scan(&admin)

				Expect(admin).To(BeTrue())
			})
		})
	})

	Describe("CreateTeam", func() {
		It("saves a team to the db", func() {
			expectedTeam := db.Team{
				Name: "avengers",
			}
			expectedSavedTeam, err := database.CreateTeam(expectedTeam)
			Expect(err).NotTo(HaveOccurred())
			Expect(expectedSavedTeam.Team).To(Equal(expectedTeam))

			savedTeam, found, err := teamDBFactory.GetTeamDB("avengers").GetTeam()
			Expect(err).NotTo(HaveOccurred())
			Expect(found).To(BeTrue())
			Expect(savedTeam).To(Equal(expectedSavedTeam))
		})

		It("saves a team to the db with basic auth", func() {
			expectedTeam := db.Team{
				Name: "avengers",
				BasicAuth: db.BasicAuth{
					BasicAuthUsername: "fake user",
					BasicAuthPassword: "no, bad",
				},
			}
			expectedSavedTeam, err := database.CreateTeam(expectedTeam)
			Expect(err).NotTo(HaveOccurred())
			Expect(expectedSavedTeam.Team.Name).To(Equal(expectedTeam.Name))

			savedTeam, found, err := teamDBFactory.GetTeamDB("avengers").GetTeam()
			Expect(err).NotTo(HaveOccurred())
			Expect(found).To(BeTrue())
			Expect(savedTeam).To(Equal(expectedSavedTeam))

			Expect(savedTeam.BasicAuthUsername).To(Equal(expectedTeam.BasicAuthUsername))
			Expect(bcrypt.CompareHashAndPassword([]byte(savedTeam.BasicAuthPassword),
				[]byte(expectedTeam.BasicAuthPassword))).To(BeNil())
		})

		It("saves a team to the db with GitHub auth", func() {
			expectedTeam := db.Team{
				Name: "avengers",
				GitHubAuth: db.GitHubAuth{
					ClientID:      "fake id",
					ClientSecret:  "some secret",
					Organizations: []string{"a", "b", "c"},
					Teams: []db.GitHubTeam{
						{
							OrganizationName: "org1",
							TeamName:         "teama",
						},
						{
							OrganizationName: "org2",
							TeamName:         "teamb",
						},
					},
					Users: []string{"user1", "user2", "user3"},
				},
			}
			expectedSavedTeam, err := database.CreateTeam(expectedTeam)
			Expect(err).NotTo(HaveOccurred())
			Expect(expectedSavedTeam.Team).To(Equal(expectedTeam))

			savedTeam, found, err := teamDBFactory.GetTeamDB("avengers").GetTeam()
			Expect(err).NotTo(HaveOccurred())
			Expect(found).To(BeTrue())
			Expect(savedTeam).To(Equal(expectedSavedTeam))

			Expect(savedTeam.ClientID).To(Equal(expectedTeam.ClientID))
			Expect(savedTeam.ClientSecret).To(Equal(expectedTeam.ClientSecret))
			Expect(savedTeam.Organizations).To(Equal(expectedTeam.Organizations))
			Expect(savedTeam.Teams).To(Equal(expectedTeam.Teams))
			Expect(savedTeam.Users).To(Equal(expectedTeam.Users))
		})
	})

	Describe("DeleteTeamByName", func() {
		Context("when the team exists", func() {
			BeforeEach(func() {
				_, err := database.CreateTeam(db.Team{
					Name: "team-name",
				})
				Expect(err).NotTo(HaveOccurred())
			})

			It("deletes the team when the name matches exactly", func() {
				err := database.DeleteTeamByName("team-name")
				Expect(err).NotTo(HaveOccurred())

				var count sql.NullInt64
				dbConn.QueryRow(`select count(1) from teams where name = 'team-name'`).Scan(&count)
				Expect(err).NotTo(HaveOccurred())
				Expect(count.Valid).To(BeTrue())
				Expect(count.Int64).To(Equal(int64(0)))
			})

			It("deletes the team when the name matches case-insensitively", func() {
				err := database.DeleteTeamByName("TEAM-name")
				Expect(err).NotTo(HaveOccurred())

				var count sql.NullInt64
				dbConn.QueryRow(`select count(1) from teams where name = 'team-name'`).Scan(&count)
				Expect(err).NotTo(HaveOccurred())
				Expect(count.Valid).To(BeTrue())
				Expect(count.Int64).To(Equal(int64(0)))
			})
		})
	})
})
