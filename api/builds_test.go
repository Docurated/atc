package api_test

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"sync"
	"time"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/onsi/gomega/ghttp"
	"github.com/pivotal-golang/lager"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
	enginefakes "github.com/concourse/atc/engine/fakes"
)

var _ = Describe("Builds API", func() {
	Describe("POST /api/v1/builds", func() {
		var plan atc.Plan

		var response *http.Response

		BeforeEach(func() {
			plan = atc.Plan{
				Task: &atc.TaskPlan{
					Config: &atc.TaskConfig{
						Run: atc.TaskRunConfig{
							Path: "ls",
						},
					},
				},
			}
		})

		JustBeforeEach(func() {
			reqPayload, err := json.Marshal(plan)
			Expect(err).NotTo(HaveOccurred())

			req, err := http.NewRequest("POST", server.URL+"/api/v1/builds", bytes.NewBuffer(reqPayload))
			Expect(err).NotTo(HaveOccurred())

			req.Header.Set("Content-Type", "application/json")

			response, err = client.Do(req)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			Context("when creating a one-off build succeeds", func() {
				BeforeEach(func() {
					buildsDB.CreateOneOffBuildReturns(db.Build{
						ID:           42,
						Name:         "1",
						JobName:      "",
						PipelineName: "",
						Status:       db.StatusStarted,
						StartTime:    time.Unix(1, 0),
						EndTime:      time.Unix(100, 0),
						ReapTime:     time.Unix(200, 0),
					}, nil)
				})

				Context("and building succeeds", func() {
					var fakeBuild *enginefakes.FakeBuild
					var resumed <-chan struct{}
					var blockForever *sync.WaitGroup

					BeforeEach(func() {
						fakeBuild = new(enginefakes.FakeBuild)

						blockForever = new(sync.WaitGroup)

						forever := blockForever
						forever.Add(1)

						r := make(chan struct{})
						resumed = r
						fakeBuild.ResumeStub = func(lager.Logger) {
							close(r)
							forever.Wait()
						}

						fakeEngine.CreateBuildReturns(fakeBuild, nil)
					})

					AfterEach(func() {
						blockForever.Done()
					})

					It("returns 201 Created", func() {
						Expect(response.StatusCode).To(Equal(http.StatusCreated))
					})

					It("returns the build", func() {
						body, err := ioutil.ReadAll(response.Body)
						Expect(err).NotTo(HaveOccurred())

						Expect(body).To(MatchJSON(`{
							"id": 42,
							"name": "1",
							"team_name": "main",
							"status": "started",
							"url": "/builds/42",
							"api_url": "/api/v1/builds/42",
							"start_time": 1,
							"end_time": 100,
							"reap_time": 200
						}`))
					})

					It("creates a one-off build and runs it asynchronously", func() {
						Expect(buildsDB.CreateOneOffBuildCallCount()).To(Equal(1))

						Expect(fakeEngine.CreateBuildCallCount()).To(Equal(1))
						_, oneOff, builtPlan := fakeEngine.CreateBuildArgsForCall(0)
						Expect(oneOff).To(Equal(db.Build{
							ID:           42,
							Name:         "1",
							TeamName:     "main",
							JobName:      "",
							PipelineName: "",
							Status:       db.StatusStarted,
							StartTime:    time.Unix(1, 0),
							EndTime:      time.Unix(100, 0),
							ReapTime:     time.Unix(200, 0),
						}))

						Expect(builtPlan).To(Equal(plan))

						<-resumed
					})
				})

				Context("and building fails", func() {
					BeforeEach(func() {
						fakeEngine.CreateBuildReturns(nil, errors.New("oh no!"))
					})

					It("returns 500 Internal Server Error", func() {
						Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
					})
				})
			})

			Context("when creating a one-off build fails", func() {
				BeforeEach(func() {
					buildsDB.CreateOneOffBuildReturns(db.Build{}, errors.New("oh no!"))
				})

				It("returns 500 Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns 401", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})

			It("does not trigger a build", func() {
				Expect(buildsDB.CreateOneOffBuildCallCount()).To(BeZero())
				Expect(fakeEngine.CreateBuildCallCount()).To(BeZero())
			})
		})
	})

	Describe("GET /api/v1/builds/:build_id", func() {
		var response *http.Response

		Context("when parsing the build_id fails", func() {
			BeforeEach(func() {
				var err error

				response, err = client.Get(server.URL + "/api/v1/builds/nope")
				Expect(err).NotTo(HaveOccurred())
			})

			It("returns Bad Request", func() {
				Expect(response.StatusCode).To(Equal(http.StatusBadRequest))
			})
		})

		Context("when parsing the build_id succeeds", func() {
			JustBeforeEach(func() {
				var err error

				response, err = client.Get(server.URL + "/api/v1/builds/1")
				Expect(err).NotTo(HaveOccurred())
			})

			Context("when calling the database fails", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, errors.New("disaster"))
				})

				It("returns 500 Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})

			Context("when the build cannot be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, nil)
				})

				It("returns Not Found", func() {
					Expect(response.StatusCode).To(Equal(http.StatusNotFound))
				})
			})

			Context("when the build can be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{
						ID:           1,
						Name:         "1",
						JobName:      "job1",
						PipelineName: "pipeline1",
						Status:       db.StatusSucceeded,
						StartTime:    time.Unix(1, 0),
						EndTime:      time.Unix(100, 0),
						ReapTime:     time.Unix(200, 0),
					}, true, nil)
				})

				It("returns 200 OK", func() {
					Expect(response.StatusCode).To(Equal(http.StatusOK))
				})

				It("returns the build with the given build_id", func() {
					Expect(buildsDB.GetBuildCallCount()).To(Equal(1))
					buildID := buildsDB.GetBuildArgsForCall(0)
					Expect(buildID).To(Equal(1))

					body, err := ioutil.ReadAll(response.Body)
					Expect(err).NotTo(HaveOccurred())

					Expect(body).To(MatchJSON(`{
						"id": 1,
						"name": "1",
						"status": "succeeded",
						"job_name": "job1",
						"pipeline_name": "pipeline1",
						"url": "/teams/main/pipelines/pipeline1/jobs/job1/builds/1",
						"team_name": "main",
						"api_url": "/api/v1/builds/1",
						"start_time": 1,
						"end_time": 100,
						"reap_time": 200
					}`))
				})
			})
		})
	})

	Describe("GET /api/v1/builds/:build_id/resources", func() {
		var response *http.Response

		Context("when the build is found", func() {
			BeforeEach(func() {
				buildsDB.GetBuildReturns(db.Build{}, true, nil)
			})

			JustBeforeEach(func() {
				var err error

				response, err = client.Get(server.URL + "/api/v1/builds/3/resources")
				Expect(err).NotTo(HaveOccurred())
			})

			It("returns 200 OK", func() {
				Expect(response.StatusCode).To(Equal(http.StatusOK))
			})

			Context("when the build inputs/ouputs are not empty", func() {
				BeforeEach(func() {
					buildsDB.GetBuildResourcesReturns([]db.BuildInput{
						{
							Name: "input1",
							VersionedResource: db.VersionedResource{
								Resource: "myresource1",
								Type:     "git",
								Version:  db.Version{"version": "value1"},
								Metadata: []db.MetadataField{
									{
										Name:  "meta1",
										Value: "value1",
									},
									{
										Name:  "meta2",
										Value: "value2",
									},
								},
								PipelineID: 42,
							},
							FirstOccurrence: true,
						},
						{
							Name: "input2",
							VersionedResource: db.VersionedResource{
								Resource:   "myresource2",
								Type:       "git",
								Version:    db.Version{"version": "value2"},
								Metadata:   []db.MetadataField{},
								PipelineID: 42,
							},
							FirstOccurrence: false,
						},
					},
						[]db.BuildOutput{
							{
								VersionedResource: db.VersionedResource{
									Resource: "myresource3",
									Version:  db.Version{"version": "value3"},
								},
							},
							{
								VersionedResource: db.VersionedResource{
									Resource: "myresource4",
									Version:  db.Version{"version": "value4"},
								},
							},
						}, nil)
				})

				It("returns the build with it's input and output versioned resources", func() {
					body, err := ioutil.ReadAll(response.Body)
					Expect(err).NotTo(HaveOccurred())

					Expect(body).To(MatchJSON(`{
							"inputs": [
								{
									"name": "input1",
									"resource": "myresource1",
									"type": "git",
									"version": {"version": "value1"},
									"metadata":[
										{
											"name": "meta1",
											"value": "value1"
										},
										{
											"name": "meta2",
											"value": "value2"
										}
									],
									"pipeline_id": 42,
									"first_occurrence": true
								},
								{
									"name": "input2",
									"resource": "myresource2",
									"type": "git",
									"version": {"version": "value2"},
									"metadata": [],
									"pipeline_id": 42,
									"first_occurrence": false
								}
							],
							"outputs": [
								{
									"id": 0,
									"pipeline_id": 0,
									"type":"",
									"metadata":null,
									"resource": "myresource3",
									"version": {"version": "value3"},
									"enabled": false
								},
								{
									"id": 0,
									"pipeline_id": 0,
									"type":"",
									"metadata":null,
									"resource": "myresource4",
									"version": {"version": "value4"},
									"enabled": false
								}
							]
						}`))
				})
			})

			Context("when the build resources error", func() {
				BeforeEach(func() {
					buildsDB.GetBuildResourcesReturns([]db.BuildInput{}, []db.BuildOutput{}, errors.New("where are my feedback?"))
				})

				It("returns internal server error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})

			Context("with an invalid build", func() {
				Context("when the lookup errors", func() {
					BeforeEach(func() {
						buildsDB.GetBuildReturns(db.Build{}, false, errors.New("Freakin' out man, I'm freakin' out!"))
					})

					It("returns internal server error", func() {
						Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
					})
				})

				Context("when the build does not exist", func() {
					BeforeEach(func() {
						buildsDB.GetBuildReturns(db.Build{}, false, nil)
					})

					It("returns internal server error", func() {
						Expect(response.StatusCode).To(Equal(http.StatusNotFound))
					})
				})
			})
		})

		Context("with an invalid build_id", func() {
			JustBeforeEach(func() {
				var err error

				response, err = client.Get(server.URL + "/api/v1/builds/nope/resources")
				Expect(err).NotTo(HaveOccurred())
			})

			It("returns internal server error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})
	})

	Describe("GET /api/v1/builds", func() {
		var response *http.Response
		var queryParams string

		returnedBuilds := []db.Build{
			{
				ID:           4,
				Name:         "2",
				JobName:      "job2",
				PipelineName: "pipeline2",
				Status:       db.StatusStarted,
				StartTime:    time.Unix(1, 0),
				EndTime:      time.Unix(100, 0),
				ReapTime:     time.Unix(300, 0),
			},
			{
				ID:           3,
				Name:         "1",
				JobName:      "job1",
				PipelineName: "pipeline1",
				Status:       db.StatusSucceeded,
				StartTime:    time.Unix(101, 0),
				EndTime:      time.Unix(200, 0),
				ReapTime:     time.Unix(400, 0),
			},
		}

		BeforeEach(func() {
			queryParams = ""
		})

		JustBeforeEach(func() {
			var err error

			response, err = client.Get(server.URL + "/api/v1/builds" + queryParams)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when no params are passed", func() {
			BeforeEach(func() {
				queryParams = ""
			})

			It("does not set defaults for since and until", func() {
				Expect(buildsDB.GetBuildsCallCount()).To(Equal(1))

				page := buildsDB.GetBuildsArgsForCall(0)
				Expect(page).To(Equal(db.Page{
					Since: 0,
					Until: 0,
					Limit: 100,
				}))
			})
		})

		Context("when all the params are passed", func() {
			BeforeEach(func() {
				queryParams = "?since=2&until=3&limit=8"
			})

			It("passes them through", func() {
				Expect(buildsDB.GetBuildsCallCount()).To(Equal(1))

				page := buildsDB.GetBuildsArgsForCall(0)
				Expect(page).To(Equal(db.Page{
					Since: 2,
					Until: 3,
					Limit: 8,
				}))
			})
		})

		Context("when getting the builds succeeds", func() {
			BeforeEach(func() {
				buildsDB.GetBuildsReturns(returnedBuilds, db.Pagination{}, nil)
			})

			It("returns 200 OK", func() {
				Expect(response.StatusCode).To(Equal(http.StatusOK))
			})

			It("returns all builds", func() {
				body, err := ioutil.ReadAll(response.Body)
				Expect(err).NotTo(HaveOccurred())

				Expect(body).To(MatchJSON(`[
					{
						"id": 4,
						"name": "2",
						"job_name": "job2",
						"pipeline_name": "pipeline2",
						"team_name": "main",
						"status": "started",
						"url": "/teams/main/pipelines/pipeline2/jobs/job2/builds/2",
						"api_url": "/api/v1/builds/4",
						"start_time": 1,
						"end_time": 100,
						"reap_time": 300
					},
					{
						"id": 3,
						"name": "1",
						"job_name": "job1",
						"pipeline_name": "pipeline1",
						"team_name": "main",
						"status": "succeeded",
						"url": "/teams/main/pipelines/pipeline1/jobs/job1/builds/1",
						"api_url": "/api/v1/builds/3",
						"start_time": 101,
						"end_time": 200,
						"reap_time": 400
					}
				]`))
			})
		})

		Context("when next/previous pages are available", func() {
			BeforeEach(func() {
				buildsDB.GetBuildsReturns(returnedBuilds, db.Pagination{
					Previous: &db.Page{Until: 4, Limit: 2},
					Next:     &db.Page{Since: 3, Limit: 2},
				}, nil)
			})

			It("returns Link headers per rfc5988", func() {
				Expect(response.Header["Link"]).To(ConsistOf([]string{
					fmt.Sprintf(`<%s/api/v1/builds?until=4&limit=2>; rel="previous"`, externalURL),
					fmt.Sprintf(`<%s/api/v1/builds?since=3&limit=2>; rel="next"`, externalURL),
				}))
			})
		})

		Context("when getting all builds fails", func() {
			BeforeEach(func() {
				buildsDB.GetBuildsReturns(nil, db.Pagination{}, errors.New("oh no!"))
			})

			It("returns 500 Internal Server Error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})
	})

	Describe("GET /api/v1/builds/:build_id/events", func() {
		var (
			request  *http.Request
			response *http.Response
		)

		BeforeEach(func() {
			var err error

			request, err = http.NewRequest("GET", server.URL+"/api/v1/builds/128/events", nil)
			Expect(err).NotTo(HaveOccurred())
		})

		JustBeforeEach(func() {
			var err error

			response, err = client.Do(request)
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			Context("when the build can be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{
						ID:      128,
						JobName: "some-job",
					}, true, nil)
				})

				It("returns 200", func() {
					Expect(response.StatusCode).To(Equal(200))
				})

				It("serves the request via the event handler", func() {
					body, err := ioutil.ReadAll(response.Body)
					Expect(err).NotTo(HaveOccurred())

					Expect(string(body)).To(Equal("fake event handler factory was here"))

					Expect(constructedEventHandler.db).To(Equal(buildsDB))
					Expect(constructedEventHandler.buildID).To(Equal(128))
				})
			})

			Context("when the build can not be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, nil)
				})

				It("returns Not Found", func() {
					Expect(response.StatusCode).To(Equal(http.StatusNotFound))
				})
			})

			Context("when calling the database fails", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, errors.New("nope"))
				})

				It("returns Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			Context("when the build can be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{
						ID:      128,
						JobName: "some-job",
					}, true, nil)
				})

				It("looks up the config from the buildsDB", func() {
					Expect(buildsDB.GetConfigByBuildIDCallCount()).To(Equal(1))
					buildID := buildsDB.GetConfigByBuildIDArgsForCall(0)
					Expect(buildID).To(Equal(128))
				})

				Context("and the build is private", func() {
					BeforeEach(func() {
						buildsDB.GetConfigByBuildIDReturns(atc.Config{
							Jobs: atc.JobConfigs{
								{Name: "some-job", Public: false},
							},
						}, 1, nil)
					})

					It("returns 401", func() {
						Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
					})
				})

				Context("and the build is public", func() {
					BeforeEach(func() {
						buildsDB.GetConfigByBuildIDReturns(atc.Config{
							Jobs: atc.JobConfigs{
								{Name: "some-job", Public: true},
							},
						}, 1, nil)
					})

					It("returns 200", func() {
						Expect(response.StatusCode).To(Equal(200))
					})

					It("serves the request via the event handler", func() {
						body, err := ioutil.ReadAll(response.Body)
						Expect(err).NotTo(HaveOccurred())

						Expect(string(body)).To(Equal("fake event handler factory was here"))

						Expect(constructedEventHandler.db).To(Equal(buildsDB))
						Expect(constructedEventHandler.buildID).To(Equal(128))
					})
				})
			})

			Context("when the build can not be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, nil)
				})

				It("returns Not Found", func() {
					Expect(response.StatusCode).To(Equal(http.StatusNotFound))
				})
			})

			Context("when calling the database fails", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, errors.New("nope"))
				})

				It("returns Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})
	})

	Describe("POST /api/v1/builds/:build_id/abort", func() {
		var (
			abortTarget *ghttp.Server

			response *http.Response
		)

		BeforeEach(func() {
			abortTarget = ghttp.NewServer()

			abortTarget.AppendHandlers(
				ghttp.VerifyRequest("POST", "/builds/some-guid/abort"),
			)
		})

		JustBeforeEach(func() {
			var err error

			req, err := http.NewRequest("POST", server.URL+"/api/v1/builds/128/abort", nil)
			Expect(err).NotTo(HaveOccurred())

			response, err = client.Do(req)
			Expect(err).NotTo(HaveOccurred())
		})

		AfterEach(func() {
			abortTarget.Close()
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			Context("when the build can be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{
						ID:     128,
						Status: db.StatusStarted,
					}, true, nil)
				})

				Context("when the engine returns a build", func() {
					var fakeBuild *enginefakes.FakeBuild

					BeforeEach(func() {
						fakeBuild = new(enginefakes.FakeBuild)
						fakeEngine.LookupBuildReturns(fakeBuild, nil)
					})

					It("aborts the build", func() {
						Expect(fakeBuild.AbortCallCount()).To(Equal(1))
					})

					Context("when aborting succeeds", func() {
						BeforeEach(func() {
							fakeBuild.AbortReturns(nil)
						})

						It("returns 204", func() {
							Expect(response.StatusCode).To(Equal(http.StatusNoContent))
						})
					})

					Context("when aborting fails", func() {
						BeforeEach(func() {
							fakeBuild.AbortReturns(errors.New("oh no!"))
						})

						It("returns 500", func() {
							Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
						})
					})
				})

				Context("when the engine returns no build", func() {
					BeforeEach(func() {
						fakeEngine.LookupBuildReturns(nil, errors.New("oh no!"))
					})

					It("returns Internal Server Error", func() {
						Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
					})
				})
			})

			Context("when the build can not be found", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, nil)
				})

				It("returns Not Found", func() {
					Expect(response.StatusCode).To(Equal(http.StatusNotFound))
				})
			})

			Context("when calling the database fails", func() {
				BeforeEach(func() {
					buildsDB.GetBuildReturns(db.Build{}, false, errors.New("nope"))
				})

				It("returns Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns 401", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})

			It("does not abort the build", func() {
				Expect(abortTarget.ReceivedRequests()).To(BeEmpty())
			})
		})
	})

	Describe("GET /api/v1/builds/:build_id/preparation", func() {
		var response *http.Response

		JustBeforeEach(func() {
			var err error
			response, err = http.Get(server.URL + "/api/v1/builds/42/preparation")
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when the build is found", func() {
			var buildPrep db.BuildPreparation

			BeforeEach(func() {
				buildPrep = db.BuildPreparation{
					BuildID:          42,
					PausedPipeline:   db.BuildPreparationStatusNotBlocking,
					PausedJob:        db.BuildPreparationStatusNotBlocking,
					MaxRunningBuilds: db.BuildPreparationStatusBlocking,
					Inputs: map[string]db.BuildPreparationStatus{
						"foo": db.BuildPreparationStatusUnknown,
						"bar": db.BuildPreparationStatusBlocking,
					},
					InputsSatisfied:     db.BuildPreparationStatusBlocking,
					MissingInputReasons: db.MissingInputReasons{"some-input": "some-reason"},
				}
				buildsDB.GetBuildPreparationReturns(buildPrep, true, nil)
			})

			It("fetches data from the db", func() {
				Expect(buildsDB.GetBuildPreparationCallCount()).To(Equal(1))
				Expect(buildsDB.GetBuildPreparationArgsForCall(0)).To(Equal(buildPrep.BuildID))
			})

			It("returns OK", func() {
				Expect(response.StatusCode).To(Equal(http.StatusOK))
			})

			It("returns the build preparation", func() {
				body, err := ioutil.ReadAll(response.Body)
				Expect(err).NotTo(HaveOccurred())

				Expect(body).To(MatchJSON(`{
					"build_id": 42,
					"paused_pipeline": "not_blocking",
					"paused_job": "not_blocking",
					"max_running_builds": "blocking",
					"inputs": {
						"foo": "unknown",
						"bar": "blocking"
					},
					"inputs_satisfied": "blocking",
					"missing_input_reasons": {
						"some-input": "some-reason"
					}
				}`))
			})
		})

		Context("when the build preparation is not found", func() {
			BeforeEach(func() {
				buildsDB.GetBuildPreparationReturns(db.BuildPreparation{}, false, nil)
			})

			It("returns Not Found", func() {
				Expect(response.StatusCode).To(Equal(http.StatusNotFound))
			})
		})

		Context("when looking up the build preparation fails", func() {
			BeforeEach(func() {
				buildsDB.GetBuildPreparationReturns(db.BuildPreparation{}, false, errors.New("ho ho ho merry festivus"))
			})

			It("returns 500 Internal Server Error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})
	})

	Describe("GET /api/v1/builds/:build_id/plan", func() {
		var publicPlan atc.PublicBuildPlan

		var response *http.Response

		BeforeEach(func() {
			var plan json.RawMessage = []byte(`"some-plan"`)

			publicPlan = atc.PublicBuildPlan{
				Schema: "some-schema",
				Plan:   &plan,
			}
		})

		JustBeforeEach(func() {
			var err error
			response, err = http.Get(server.URL + "/api/v1/builds/42/plan")
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when the build is found", func() {
			var engineBuild *enginefakes.FakeBuild

			BeforeEach(func() {
				buildsDB.GetBuildReturns(db.Build{
					ID: 42,
				}, true, nil)

				engineBuild = new(enginefakes.FakeBuild)
				fakeEngine.LookupBuildReturns(engineBuild, nil)
			})

			Context("when the build returns a plan", func() {
				BeforeEach(func() {
					engineBuild.PublicPlanReturns(publicPlan, true, nil)
				})

				It("returns OK", func() {
					Expect(response.StatusCode).To(Equal(http.StatusOK))
				})

				It("returns the plan", func() {
					body, err := ioutil.ReadAll(response.Body)
					Expect(err).NotTo(HaveOccurred())

					Expect(body).To(MatchJSON(`{
						"schema": "some-schema",
						"plan": "some-plan"
					}`))
				})
			})

			Context("when the build has no plan", func() {
				BeforeEach(func() {
					engineBuild.PublicPlanReturns(atc.PublicBuildPlan{}, false, nil)
				})

				It("returns Not Found", func() {
					Expect(response.StatusCode).To(Equal(http.StatusNotFound))
				})
			})

			Context("when the build fails to return a plan", func() {
				BeforeEach(func() {
					engineBuild.PublicPlanReturns(atc.PublicBuildPlan{}, false, errors.New("nope"))
				})

				It("returns 500 Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})

		Context("when the build is not found", func() {
			BeforeEach(func() {
				buildsDB.GetBuildReturns(db.Build{}, false, nil)
			})

			It("returns Not Found", func() {
				Expect(response.StatusCode).To(Equal(http.StatusNotFound))
			})
		})

		Context("when looking up the build fails", func() {
			BeforeEach(func() {
				buildsDB.GetBuildReturns(db.Build{}, false, errors.New("oh no!"))
			})

			It("returns 500 Internal Server Error", func() {
				Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
			})
		})
	})
})
