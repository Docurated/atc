package db_test

import (
	"time"

	"golang.org/x/crypto/bcrypt"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
	"github.com/lib/pq"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("TeamDB", func() {
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

		team := db.Team{Name: "team-name"}
		_, err := database.CreateTeam(team)
		Expect(err).NotTo(HaveOccurred())
	})

	AfterEach(func() {
		err := dbConn.Close()
		Expect(err).NotTo(HaveOccurred())

		err = listener.Close()
		Expect(err).NotTo(HaveOccurred())
	})

	Context("when constructed with a team name that exactly matches a saved team", func() {
		var teamDB db.TeamDB
		BeforeEach(func() {
			teamDB = teamDBFactory.GetTeamDB("team-name")
		})

		Describe("GetTeam", func() {
			It("returns the saved team", func() {
				actualTeam, found, err := teamDB.GetTeam()
				Expect(err).NotTo(HaveOccurred())
				Expect(found).To(BeTrue())
				Expect(actualTeam.Name).To(Equal("team-name"))
			})
		})

		Describe("GetPipielineByName", func() {
			var savedPipeline db.SavedPipeline
			BeforeEach(func() {
				var err error
				savedPipeline, _, err = teamDB.SaveConfig("pipeline-name", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())

				team := db.Team{Name: "other-team-name"}
				_, err = database.CreateTeam(team)
				Expect(err).NotTo(HaveOccurred())
				otherTeamDB := teamDBFactory.GetTeamDB("other-team-name")
				_, _, err = otherTeamDB.SaveConfig("pipeline-name", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())
			})

			It("returns the pipeline with the name that belongs to the team", func() {
				actualPipeline, err := teamDB.GetPipelineByName("pipeline-name")
				Expect(err).NotTo(HaveOccurred())
				Expect(actualPipeline).To(Equal(savedPipeline))
			})
		})

		Describe("GetPipelines", func() {
			var savedPipeline1 db.SavedPipeline
			var savedPipeline2 db.SavedPipeline

			BeforeEach(func() {
				var err error
				savedPipeline1, _, err = teamDB.SaveConfig("pipeline-name-a", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())

				savedPipeline2, _, err = teamDB.SaveConfig("pipeline-name-b", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())

				team := db.Team{Name: "other-team-name"}
				_, err = database.CreateTeam(team)
				Expect(err).NotTo(HaveOccurred())
				otherTeamDB := teamDBFactory.GetTeamDB("other-team-name")

				_, _, err = otherTeamDB.SaveConfig("other-team-pipeline-name-a", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())

				_, _, err = otherTeamDB.SaveConfig("other-team-pipeline-name-b", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())
			})

			It("returns pipelines that belong to team", func() {
				savedPipelines, err := teamDB.GetPipelines()
				Expect(err).NotTo(HaveOccurred())
				Expect(savedPipelines).To(HaveLen(2))
				Expect(savedPipelines).To(ConsistOf(savedPipeline1, savedPipeline2))
			})
		})

		Describe("OrderPipelines", func() {
			var otherTeamDB db.TeamDB
			var savedPipeline1 db.SavedPipeline
			var savedPipeline2 db.SavedPipeline
			var otherTeamSavedPipeline1 db.SavedPipeline
			var otherTeamSavedPipeline2 db.SavedPipeline

			BeforeEach(func() {
				var err error
				savedPipeline1, _, err = teamDB.SaveConfig("pipeline-name-a", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())
				savedPipeline2, _, err = teamDB.SaveConfig("pipeline-name-b", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())

				team := db.Team{Name: "other-team-name"}
				_, err = database.CreateTeam(team)
				Expect(err).NotTo(HaveOccurred())
				otherTeamDB = teamDBFactory.GetTeamDB("other-team-name")

				otherTeamSavedPipeline1, _, err = otherTeamDB.SaveConfig("pipeline-name-a", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())
				otherTeamSavedPipeline2, _, err = otherTeamDB.SaveConfig("pipeline-name-b", atc.Config{}, 0, db.PipelineUnpaused)
				Expect(err).NotTo(HaveOccurred())
			})

			It("orders pipelines that belong to team", func() {
				err := teamDB.OrderPipelines([]string{"pipeline-name-b", "pipeline-name-a"})
				Expect(err).NotTo(HaveOccurred())

				err = otherTeamDB.OrderPipelines([]string{"pipeline-name-a", "pipeline-name-b"})
				Expect(err).NotTo(HaveOccurred())

				orderedPipelines, err := teamDB.GetPipelines()
				Expect(err).NotTo(HaveOccurred())
				Expect(orderedPipelines).To(HaveLen(2))
				Expect(orderedPipelines[0].ID).To(Equal(savedPipeline2.ID))
				Expect(orderedPipelines[1].ID).To(Equal(savedPipeline1.ID))

				otherTeamOrderedPipelines, err := otherTeamDB.GetPipelines()
				Expect(err).NotTo(HaveOccurred())
				Expect(otherTeamOrderedPipelines).To(HaveLen(2))
				Expect(otherTeamOrderedPipelines[0].ID).To(Equal(otherTeamSavedPipeline1.ID))
				Expect(otherTeamOrderedPipelines[1].ID).To(Equal(otherTeamSavedPipeline2.ID))
			})
		})

		Describe("UpdateBasicAuth", func() {
			var basicAuthTeam, gitHubAuthTeam db.Team
			BeforeEach(func() {
				basicAuthTeam = db.Team{
					Name: "team-name",
					BasicAuth: db.BasicAuth{
						BasicAuthUsername: "fake user",
						BasicAuthPassword: "no, bad",
					},
				}

				gitHubAuthTeam = db.Team{
					Name: "team-name",
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
			})

			It("saves basic auth team info without over writing the github auth", func() {
				_, err := teamDB.UpdateGitHubAuth(gitHubAuthTeam.GitHubAuth)
				Expect(err).NotTo(HaveOccurred())

				savedTeam, err := teamDB.UpdateBasicAuth(basicAuthTeam.BasicAuth)
				Expect(err).NotTo(HaveOccurred())

				Expect(savedTeam.ClientID).To(Equal(gitHubAuthTeam.ClientID))
				Expect(savedTeam.ClientSecret).To(Equal(gitHubAuthTeam.ClientSecret))
				Expect(savedTeam.Organizations).To(Equal(gitHubAuthTeam.Organizations))
				Expect(savedTeam.Teams).To(Equal(gitHubAuthTeam.Teams))
				Expect(savedTeam.Users).To(Equal(gitHubAuthTeam.Users))
			})

			It("saves basic auth team info to the existing team", func() {
				savedTeam, err := teamDB.UpdateBasicAuth(basicAuthTeam.BasicAuth)
				Expect(err).NotTo(HaveOccurred())

				Expect(savedTeam.BasicAuthUsername).To(Equal(basicAuthTeam.BasicAuthUsername))
				Expect(bcrypt.CompareHashAndPassword([]byte(savedTeam.BasicAuthPassword),
					[]byte(basicAuthTeam.BasicAuthPassword))).To(BeNil())
			})

			It("nulls basic auth when has a blank username", func() {
				basicAuthTeam.BasicAuthUsername = ""
				savedTeam, err := teamDB.UpdateBasicAuth(basicAuthTeam.BasicAuth)
				Expect(err).NotTo(HaveOccurred())

				Expect(savedTeam.BasicAuth.BasicAuthUsername).To(BeEmpty())
				Expect(savedTeam.BasicAuth.BasicAuthPassword).To(BeEmpty())
			})

			It("nulls basic auth when has a blank password", func() {
				basicAuthTeam.BasicAuthPassword = ""
				savedTeam, err := teamDB.UpdateBasicAuth(basicAuthTeam.BasicAuth)
				Expect(err).NotTo(HaveOccurred())

				Expect(savedTeam.BasicAuth.BasicAuthUsername).To(BeEmpty())
				Expect(savedTeam.BasicAuth.BasicAuthPassword).To(BeEmpty())
			})
		})

		Describe("UpdateGitHubAuth", func() {
			var basicAuthTeam, gitHubAuthTeam db.Team
			var gitHubAuth db.GitHubAuth

			BeforeEach(func() {
				basicAuthTeam = db.Team{
					Name: "avengers",
					BasicAuth: db.BasicAuth{
						BasicAuthUsername: "fake user",
						BasicAuthPassword: "no, bad",
					},
				}

				gitHubAuth = db.GitHubAuth{
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
				}

				gitHubAuthTeam = db.Team{
					Name:       "avengers",
					GitHubAuth: gitHubAuth,
				}
			})

			Context("when the team exists", func() {
				BeforeEach(func() {
					expectedTeam := db.Team{
						Name: "avengers",
					}
					_, err := database.CreateTeam(expectedTeam)
					Expect(err).NotTo(HaveOccurred())
				})

				It("saves github auth team info to the existing team", func() {
					savedTeam, err := teamDB.UpdateGitHubAuth(gitHubAuthTeam.GitHubAuth)
					Expect(err).NotTo(HaveOccurred())

					Expect(savedTeam.GitHubAuth).To(Equal(gitHubAuth))
					Expect(savedTeam.ClientID).To(Equal(gitHubAuthTeam.ClientID))
					Expect(savedTeam.ClientSecret).To(Equal(gitHubAuthTeam.ClientSecret))
					Expect(savedTeam.Organizations).To(Equal(gitHubAuthTeam.Organizations))
					Expect(savedTeam.Teams).To(Equal(gitHubAuthTeam.Teams))
					Expect(savedTeam.Users).To(Equal(gitHubAuthTeam.Users))
				})

				It("nulls github auth when has a blank clientSecret", func() {
					gitHubAuthTeam.ClientSecret = ""
					savedTeam, err := teamDB.UpdateGitHubAuth(gitHubAuthTeam.GitHubAuth)
					Expect(err).NotTo(HaveOccurred())

					Expect(savedTeam.GitHubAuth).To(Equal(db.GitHubAuth{}))
					Expect(savedTeam.ClientID).To(BeEmpty())
					Expect(savedTeam.ClientSecret).To(BeEmpty())
					Expect(savedTeam.Organizations).To(BeEmpty())
					Expect(savedTeam.Teams).To(BeEmpty())
					Expect(savedTeam.Users).To(BeEmpty())
				})

				It("nulls github auth when has a blank clientID", func() {
					gitHubAuthTeam.ClientID = ""
					savedTeam, err := teamDB.UpdateGitHubAuth(gitHubAuthTeam.GitHubAuth)
					Expect(err).NotTo(HaveOccurred())

					Expect(savedTeam.GitHubAuth).To(Equal(db.GitHubAuth{}))
					Expect(savedTeam.ClientID).To(BeEmpty())
					Expect(savedTeam.ClientSecret).To(BeEmpty())
					Expect(savedTeam.Organizations).To(BeEmpty())
					Expect(savedTeam.Teams).To(BeEmpty())
					Expect(savedTeam.Users).To(BeEmpty())
				})

				It("saves github auth team info without over writing the basic auth", func() {
					_, err := teamDB.UpdateBasicAuth(basicAuthTeam.BasicAuth)
					Expect(err).NotTo(HaveOccurred())

					savedTeam, err := teamDB.UpdateGitHubAuth(gitHubAuthTeam.GitHubAuth)
					Expect(err).NotTo(HaveOccurred())

					Expect(savedTeam.BasicAuthUsername).To(Equal(basicAuthTeam.BasicAuthUsername))
					Expect(bcrypt.CompareHashAndPassword([]byte(savedTeam.BasicAuthPassword),
						[]byte(basicAuthTeam.BasicAuthPassword))).To(BeNil())
				})
			})
		})
	})

	Context("when constructed with a team name that matches a saved team case-insensitively", func() {
		var teamDB db.TeamDB
		BeforeEach(func() {
			teamDB = teamDBFactory.GetTeamDB("TEAM-name")
		})

		Describe("GetTeam", func() {
			It("returns the saved team", func() {
				actualTeam, found, err := teamDB.GetTeam()
				Expect(err).NotTo(HaveOccurred())
				Expect(found).To(BeTrue())
				Expect(actualTeam.Name).To(Equal("team-name"))
			})
		})

		Describe("UpdateBasicAuth", func() {
			var basicAuth db.BasicAuth
			BeforeEach(func() {
				basicAuth = db.BasicAuth{
					BasicAuthUsername: "fake user",
					BasicAuthPassword: "no, bad",
				}
			})

			It("saves basic auth team info to the existing team", func() {
				savedTeam, err := teamDB.UpdateBasicAuth(basicAuth)
				Expect(err).NotTo(HaveOccurred())

				Expect(savedTeam.BasicAuthUsername).To(Equal(basicAuth.BasicAuthUsername))
				Expect(bcrypt.CompareHashAndPassword([]byte(savedTeam.BasicAuthPassword),
					[]byte(basicAuth.BasicAuthPassword))).To(BeNil())
			})
		})

		Describe("UpdateGitHubAuth", func() {
			var gitHubAuth db.GitHubAuth

			BeforeEach(func() {
				gitHubAuth = db.GitHubAuth{
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
				}
			})

			It("saves github auth team info to the existing team", func() {
				savedTeam, err := teamDB.UpdateGitHubAuth(gitHubAuth)
				Expect(err).NotTo(HaveOccurred())

				Expect(savedTeam.GitHubAuth).To(Equal(gitHubAuth))
				Expect(savedTeam.ClientID).To(Equal(gitHubAuth.ClientID))
				Expect(savedTeam.ClientSecret).To(Equal(gitHubAuth.ClientSecret))
				Expect(savedTeam.Organizations).To(Equal(gitHubAuth.Organizations))
				Expect(savedTeam.Teams).To(Equal(gitHubAuth.Teams))
				Expect(savedTeam.Users).To(Equal(gitHubAuth.Users))
			})
		})
	})

	Context("when constructed with a team name that does not match any team", func() {
		var teamDB db.TeamDB

		BeforeEach(func() {
			teamDB = teamDBFactory.GetTeamDB("nonexistent-team")
		})

		Describe("GetTeam", func() {
			It("returns false with no error when the team does not exist", func() {
				_, found, err := teamDB.GetTeam()
				Expect(err).NotTo(HaveOccurred())
				Expect(found).To(BeFalse())
			})
		})
	})
})
