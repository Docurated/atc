package api_test

import (
	"bytes"
	"errors"
	"io"
	"io/ioutil"
	"net/http"
	"time"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/concourse/atc"
	dbfakes "github.com/concourse/atc/db/fakes"

	"github.com/concourse/atc/db"
	"github.com/concourse/atc/db/algorithm"
)

var _ = Describe("Pipelines API", func() {
	var pipelineDB *dbfakes.FakePipelineDB
	var expectedSavedPipeline db.SavedPipeline

	BeforeEach(func() {
		pipelineDB = new(dbfakes.FakePipelineDB)
		pipelineDBFactory.BuildReturns(pipelineDB)
		expectedSavedPipeline = db.SavedPipeline{}
		teamDB.GetPipelineByNameReturns(expectedSavedPipeline, nil)
	})

	Describe("GET /api/v1/teams/:team_name/pipelines", func() {
		var response *http.Response

		BeforeEach(func() {
			teamDB.GetPipelinesReturns([]db.SavedPipeline{
				{
					ID:     1,
					Paused: false,
					Pipeline: db.Pipeline{
						Name: "a-pipeline",
					},
				},
				{
					ID:     2,
					Paused: true,
					Pipeline: db.Pipeline{
						Name: "another-pipeline",
					},
				},
			}, nil)

			teamDB.GetConfigStub = func(pipelineName string) (atc.Config, atc.RawConfig, db.ConfigVersion, error) {
				if pipelineName == "a-pipeline" {
					return atc.Config{
						Groups: atc.GroupConfigs{
							{
								Name:      "group1",
								Jobs:      []string{"job1", "job2"},
								Resources: []string{"resource1", "resource2"},
							},
						},
					}, atc.RawConfig(""), 42, nil
				} else if pipelineName == "another-pipeline" {
					return atc.Config{
						Groups: atc.GroupConfigs{
							{
								Name:      "group2",
								Jobs:      []string{"job3", "job4"},
								Resources: []string{"resource3", "resource4"},
							},
						},
					}, atc.RawConfig(""), 42, nil
				}

				panic("don't know what's going on")
			}
		})

		JustBeforeEach(func() {
			req, err := http.NewRequest("GET", server.URL+"/api/v1/teams/a-team/pipelines", nil)
			Expect(err).NotTo(HaveOccurred())

			req.Header.Set("Content-Type", "application/json")

			response, err = client.Do(req)
			Expect(err).NotTo(HaveOccurred())
		})

		It("returns 200 OK", func() {
			Expect(response.StatusCode).To(Equal(http.StatusOK))
		})

		It("returns application/json", func() {
			Expect(response.Header.Get("Content-Type")).To(Equal("application/json"))
		})

		It("constructs teamDB with provided team name", func() {
			Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
			Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
		})

		It("returns all active pipelines", func() {
			body, err := ioutil.ReadAll(response.Body)
			Expect(err).NotTo(HaveOccurred())

			Expect(body).To(MatchJSON(`[
      {
        "name": "a-pipeline",
        "url": "/teams/a-team/pipelines/a-pipeline",
				"paused": false,
				"team_name": "a-team",
				"groups": [
					{
						"name": "group1",
						"jobs": ["job1", "job2"],
						"resources": ["resource1", "resource2"]
					}
				]
      },{
        "name": "another-pipeline",
        "url": "/teams/a-team/pipelines/another-pipeline",
				"paused": true,
				"team_name": "a-team",
				"groups": [
					{
						"name": "group2",
						"jobs": ["job3", "job4"],
						"resources": ["resource3", "resource4"]
					}
				]
      }]`))
		})

		Context("when the call to get active pipelines fails", func() {
			BeforeEach(func() {
				teamDB.GetPipelinesReturns(nil, errors.New("disaster"))
			})

			It("returns 500 internal server error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})

		Context("when the call to get a pipeline's config fails", func() {
			BeforeEach(func() {
				teamDB.GetConfigReturns(atc.Config{}, atc.RawConfig(""), 0, errors.New("disaster"))
			})

			It("returns 500 internal server error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})
	})

	Describe("GET /api/v1/teams/:team_name/pipelines/:pipeline_name", func() {
		var response *http.Response

		BeforeEach(func() {
			teamDB.GetPipelineByNameReturns(db.SavedPipeline{
				ID:     1,
				Paused: false,
				Pipeline: db.Pipeline{
					Name: "some-specific-pipeline",
				},
			}, nil)

			teamDB.GetConfigReturns(atc.Config{
				Groups: atc.GroupConfigs{
					{
						Name:      "group1",
						Jobs:      []string{"job1", "job2"},
						Resources: []string{"resource1", "resource2"},
					},
					{
						Name:      "group2",
						Jobs:      []string{"job3", "job4"},
						Resources: []string{"resource3", "resource4"},
					},
				},
			}, atc.RawConfig(""), 42, nil)
		})

		JustBeforeEach(func() {
			req, err := http.NewRequest("GET", server.URL+"/api/v1/teams/a-team/pipelines/some-specific-pipeline", nil)
			Expect(err).NotTo(HaveOccurred())

			req.Header.Set("Content-Type", "application/json")

			response, err = client.Do(req)
			Expect(err).NotTo(HaveOccurred())
		})

		It("looks up the pipeline in the db via the url param", func() {
			Expect(teamDB.GetPipelineByNameCallCount()).To(Equal(1))
			Expect(teamDB.GetPipelineByNameArgsForCall(0)).To(Equal("some-specific-pipeline"))
		})

		It("tries to get the config scoped to team and pipeline", func() {
			Expect(teamDB.GetConfigCallCount()).To(Equal(1))
			Expect(teamDB.GetConfigArgsForCall(0)).To(Equal("some-specific-pipeline"))
		})

		It("returns 200 ok", func() {
			Expect(response.StatusCode).To(Equal(http.StatusOK))
		})

		It("returns application/json", func() {
			Expect(response.Header.Get("Content-Type")).To(Equal("application/json"))
		})

		It("constructs teamDB with provided team name", func() {
			Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
			Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
		})

		It("returns a pipeline JSON", func() {
			body, err := ioutil.ReadAll(response.Body)
			Expect(err).NotTo(HaveOccurred())

			Expect(body).To(MatchJSON(`
      {
        "name": "some-specific-pipeline",
        "url": "/teams/a-team/pipelines/some-specific-pipeline",
				"paused": false,
				"team_name": "a-team",
				"groups": [
					{
						"name": "group1",
						"jobs": ["job1", "job2"],
						"resources": ["resource1", "resource2"]
					},
					{
						"name": "group2",
						"jobs": ["job3", "job4"],
						"resources": ["resource3", "resource4"]
					}
				]
      }`))
		})

		Context("when the call to get pipeline fails", func() {
			BeforeEach(func() {
				teamDB.GetPipelineByNameReturns(db.SavedPipeline{}, errors.New("disaster"))
			})

			It("returns 500 error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})

		Context("when the call to get the pipeline config fails", func() {
			BeforeEach(func() {
				teamDB.GetConfigReturns(atc.Config{}, atc.RawConfig(""), 0, errors.New("disaster"))
			})

			It("returns 500 error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})
	})

	Describe("DELETE /api/v1/teams/:team_name/pipelines/:pipeline_name", func() {
		var response *http.Response

		JustBeforeEach(func() {
			pipelineName := "a-pipeline-name"
			req, err := http.NewRequest("DELETE", server.URL+"/api/v1/teams/a-team/pipelines/"+pipelineName, nil)
			Expect(err).NotTo(HaveOccurred())

			req.Header.Set("Content-Type", "application/json")

			response, err = client.Do(req)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when the user is logged in", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			It("returns 204 No Content", func() {
				Expect(response.StatusCode).To(Equal(http.StatusNoContent))
			})

			It("constructs teamDB with provided team name", func() {
				Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
				Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
			})

			It("injects the proper pipelineDB", func() {
				pipelineName := teamDB.GetPipelineByNameArgsForCall(0)
				Expect(pipelineName).To(Equal("a-pipeline-name"))
				Expect(pipelineDBFactory.BuildCallCount()).To(Equal(1))
				actualSavedPipeline := pipelineDBFactory.BuildArgsForCall(0)
				Expect(actualSavedPipeline).To(Equal(expectedSavedPipeline))
			})

			It("deletes the named pipeline from the database", func() {
				Expect(pipelineDB.DestroyCallCount()).To(Equal(1))
			})

			Context("when an error occurs destroying the pipeline", func() {
				BeforeEach(func() {
					err := errors.New("disaster!")
					pipelineDB.DestroyReturns(err)
				})

				It("returns a 500 Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when the user is not logged in", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns Unauthorized", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})
		})
	})

	Describe("PUT /api/v1/teams/:team_name/pipelines/:pipeline_name/pause", func() {
		var response *http.Response

		JustBeforeEach(func() {
			var err error

			request, err := http.NewRequest("PUT", server.URL+"/api/v1/teams/a-team/pipelines/a-pipeline/pause", nil)
			Expect(err).NotTo(HaveOccurred())

			response, err = client.Do(request)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			It("constructs teamDB with provided team name", func() {
				Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
				Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
			})

			It("injects the proper pipelineDB", func() {
				pipelineName := teamDB.GetPipelineByNameArgsForCall(0)
				Expect(pipelineName).To(Equal("a-pipeline"))
				Expect(pipelineDBFactory.BuildCallCount()).To(Equal(1))
				actualSavedPipeline := pipelineDBFactory.BuildArgsForCall(0)
				Expect(actualSavedPipeline).To(Equal(expectedSavedPipeline))
			})

			Context("when pausing the pipeline succeeds", func() {
				BeforeEach(func() {
					pipelineDB.PauseReturns(nil)
				})

				It("returns 200", func() {
					Expect(response.StatusCode).To(Equal(http.StatusOK))
				})
			})

			Context("when pausing the pipeline fails", func() {
				BeforeEach(func() {
					pipelineDB.PauseReturns(errors.New("welp"))
				})

				It("returns 500", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns Unauthorized", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})
		})
	})

	Describe("PUT /api/v1/teams/:team_name/pipelines/:pipeline_name/unpause", func() {
		var response *http.Response

		JustBeforeEach(func() {
			var err error

			request, err := http.NewRequest("PUT", server.URL+"/api/v1/teams/a-team/pipelines/a-pipeline/unpause", nil)
			Expect(err).NotTo(HaveOccurred())

			response, err = client.Do(request)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			It("constructs teamDB with provided team name", func() {
				Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
				Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
			})

			It("injects the proper pipelineDB", func() {
				pipelineName := teamDB.GetPipelineByNameArgsForCall(0)
				Expect(pipelineName).To(Equal("a-pipeline"))
				Expect(pipelineDBFactory.BuildCallCount()).To(Equal(1))
				actualSavedPipeline := pipelineDBFactory.BuildArgsForCall(0)
				Expect(actualSavedPipeline).To(Equal(expectedSavedPipeline))
			})

			Context("when unpausing the pipeline succeeds", func() {
				BeforeEach(func() {
					pipelineDB.UnpauseReturns(nil)
				})

				It("returns 200", func() {
					Expect(response.StatusCode).To(Equal(http.StatusOK))
				})
			})

			Context("when unpausing the pipeline fails", func() {
				BeforeEach(func() {
					pipelineDB.UnpauseReturns(errors.New("welp"))
				})

				It("returns 500", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns Unauthorized", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})
		})
	})

	Describe("PUT /api/v1/teams/:team_name/pipelines/ordering", func() {
		var response *http.Response
		var body io.Reader

		BeforeEach(func() {
			body = bytes.NewBufferString(`
				[
					"a-pipeline",
					"another-pipeline",
					"yet-another-pipeline",
					"one-final-pipeline",
					"just-kidding"
				]
			`)
		})

		JustBeforeEach(func() {
			var err error

			request, err := http.NewRequest("PUT", server.URL+"/api/v1/teams/a-team/pipelines/ordering", body)
			Expect(err).NotTo(HaveOccurred())

			response, err = client.Do(request)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			Context("with invalid json", func() {
				BeforeEach(func() {
					body = bytes.NewBufferString(`{}`)
				})

				It("returns 400", func() {
					Expect(response.StatusCode).To(Equal(http.StatusBadRequest))
				})
			})

			It("constructs teamDB with provided team name", func() {
				Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
				Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
			})

			Context("when ordering the pipelines succeeds", func() {
				BeforeEach(func() {
					teamDB.OrderPipelinesReturns(nil)
				})

				It("orders the pipelines", func() {
					Expect(teamDB.OrderPipelinesCallCount()).To(Equal(1))
					pipelineNames := teamDB.OrderPipelinesArgsForCall(0)
					Expect(pipelineNames).To(Equal(
						[]string{
							"a-pipeline",
							"another-pipeline",
							"yet-another-pipeline",
							"one-final-pipeline",
							"just-kidding",
						},
					))

				})

				It("returns 200", func() {
					Expect(response.StatusCode).To(Equal(http.StatusOK))
				})
			})

			Context("when ordering the pipelines fails", func() {
				BeforeEach(func() {
					teamDB.OrderPipelinesReturns(errors.New("welp"))
				})

				It("returns 500", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns Unauthorized", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})
		})
	})

	Describe("GET /api/v1/teams/:team_name/pipelines/:pipeline_name/versions-db", func() {
		var response *http.Response

		JustBeforeEach(func() {
			var err error

			request, err := http.NewRequest("GET", server.URL+"/api/v1/teams/a-team/pipelines/a-pipeline/versions-db", nil)
			Expect(err).NotTo(HaveOccurred())

			response, err = client.Do(request)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
				//construct Version db

				pipelineDB.LoadVersionsDBReturns(
					&algorithm.VersionsDB{
						ResourceVersions: []algorithm.ResourceVersion{
							{
								VersionID:  73,
								ResourceID: 127,
								CheckOrder: 123,
							},
						},
						BuildOutputs: []algorithm.BuildOutput{
							{
								ResourceVersion: algorithm.ResourceVersion{
									VersionID:  73,
									ResourceID: 127,
									CheckOrder: 123,
								},
								BuildID: 66,
								JobID:   13,
							},
						},
						BuildInputs: []algorithm.BuildInput{
							{
								ResourceVersion: algorithm.ResourceVersion{
									VersionID:  66,
									ResourceID: 77,
									CheckOrder: 88,
								},
								BuildID: 66,
								JobID:   13,
							},
						},
						JobIDs: map[string]int{
							"bad-luck-job": 13,
						},
						ResourceIDs: map[string]int{
							"resource-127": 127,
						},
						CachedAt: time.Unix(42, 0).UTC(),
					},
					nil,
				)
			})

			It("constructs teamDB with provided team name", func() {
				Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
				Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
			})

			It("returns 200", func() {
				Expect(response.StatusCode).To(Equal(http.StatusOK))
			})

			It("returns application/json", func() {
				Expect(response.Header.Get("Content-Type")).To(Equal("application/json"))
			})

			It("returns a json representation of all the versions in the pipeline", func() {
				body, err := ioutil.ReadAll(response.Body)
				Expect(err).NotTo(HaveOccurred())

				Expect(body).To(MatchJSON(`{
				"ResourceVersions": [
					{
						"VersionID": 73,
						"ResourceID": 127,
						"CheckOrder": 123
			    }
				],
				"BuildOutputs": [
					{
						"VersionID": 73,
						"ResourceID": 127,
						"BuildID": 66,
						"JobID": 13,
						"CheckOrder": 123
					}
				],
				"BuildInputs": [
					{
						"VersionID": 66,
						"ResourceID": 77,
						"BuildID": 66,
						"JobID": 13,
						"CheckOrder": 88
					}
				],
				"JobIDs": {
						"bad-luck-job": 13
				},
				"ResourceIDs": {
					"resource-127": 127
				},
				"CachedAt": "1970-01-01T00:00:42Z"
				}`))
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns Unauthorized", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})
		})
	})

	Describe("PUT /api/v1/teams/:team_name/pipelines/:pipeline_name/rename", func() {
		var response *http.Response

		BeforeEach(func() {
			authValidator.IsAuthenticatedReturns(true)
		})

		JustBeforeEach(func() {
			var err error

			request, err := http.NewRequest("PUT", server.URL+"/api/v1/teams/a-team/pipelines/a-pipeline/rename", bytes.NewBufferString(`{"name":"some-new-name"}`))
			Expect(err).NotTo(HaveOccurred())

			response, err = client.Do(request)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {
			It("constructs teamDB with provided team name", func() {
				Expect(teamDBFactory.GetTeamDBCallCount()).To(Equal(1))
				Expect(teamDBFactory.GetTeamDBArgsForCall(0)).To(Equal("a-team"))
			})

			It("injects the proper pipelineDB", func() {
				pipelineName := teamDB.GetPipelineByNameArgsForCall(0)
				Expect(pipelineName).To(Equal("a-pipeline"))
				Expect(pipelineDBFactory.BuildCallCount()).To(Equal(1))
				actualSavedPipeline := pipelineDBFactory.BuildArgsForCall(0)
				Expect(actualSavedPipeline).To(Equal(expectedSavedPipeline))
			})

			It("returns 204", func() {
				Expect(response.StatusCode).To(Equal(http.StatusNoContent))
			})

			It("renames the pipeline to the name provided", func() {
				Expect(pipelineDB.UpdateNameCallCount()).To(Equal(1))
				Expect(pipelineDB.UpdateNameArgsForCall(0)).To(Equal("some-new-name"))
			})

			Context("when an error occurs on update", func() {
				BeforeEach(func() {
					pipelineDB.UpdateNameReturns(errors.New("whoops"))
				})

				It("returns a 500 internal server error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
					Expect(logger.LogMessages()).To(ContainElement("callbacks.call-to-update-pipeline-name-failed"))
				})
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns Unauthorized", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})
		})
	})
})
