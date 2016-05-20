package api_test

import (
	"errors"
	"io/ioutil"
	"net/http"
	"time"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Pipelines API", func() {
	Describe("GET /api/v1/volumes", func() {
		var response *http.Response

		JustBeforeEach(func() {
			var err error

			response, err = client.Get(server.URL + "/api/v1/volumes")
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when not authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(false)
			})

			It("returns 401 Unauthorized", func() {
				Expect(response.StatusCode).To(Equal(http.StatusUnauthorized))
			})
		})

		Context("when authenticated", func() {
			BeforeEach(func() {
				authValidator.IsAuthenticatedReturns(true)
			})

			Context("when getting all volumes succeeds", func() {
				BeforeEach(func() {
					someVersion := "some-version"
					volumesDB.GetVolumesReturns([]db.SavedVolume{
						{
							ID:        3,
							ExpiresIn: 2 * time.Minute,
							Volume: db.Volume{
								WorkerName: "some-worker",
								TTL:        10 * time.Minute,
								Handle:     "some-resource-cache-handle",
								Identifier: db.VolumeIdentifier{
									ResourceCache: &db.ResourceCacheIdentifier{
										ResourceVersion: atc.Version{"a": "b", "c": "d"},
										ResourceHash:    "some-hash",
									},
								},
								Size: 1024,
							},
						},
						{
							ID:        1,
							ExpiresIn: 23 * time.Hour,
							Volume: db.Volume{
								WorkerName: "some-worker",
								TTL:        24 * time.Hour,
								Handle:     "some-import-handle",
								Identifier: db.VolumeIdentifier{
									Import: &db.ImportIdentifier{
										WorkerName: "some-worker",
										Path:       "some-path",
										Version:    &someVersion,
									},
								},
								Size: 2048,
							},
						},
						{
							ID:        1,
							ExpiresIn: 23 * time.Hour,
							Volume: db.Volume{
								WorkerName: "some-other-worker",
								TTL:        24 * time.Hour,
								Handle:     "some-output-handle",
								Identifier: db.VolumeIdentifier{
									Output: &db.OutputIdentifier{
										Name: "some-output",
									},
								},
								Size: 4096,
							},
						},
						{
							ID:        1,
							ExpiresIn: time.Duration(0),
							Volume: db.Volume{
								WorkerName: "some-worker",
								TTL:        time.Duration(0),
								Handle:     "some-cow-handle",
								Identifier: db.VolumeIdentifier{
									COW: &db.COWIdentifier{
										ParentVolumeHandle: "some-parent-volume-handle",
									},
								},
								Size: 8192,
							},
						},
					}, nil)
				})

				It("returns 200 OK", func() {
					Expect(response.StatusCode).To(Equal(http.StatusOK))
				})

				It("returns all volumes", func() {
					body, err := ioutil.ReadAll(response.Body)
					Expect(err).NotTo(HaveOccurred())

					Expect(body).To(MatchJSON(`[
						{
							"id": "some-resource-cache-handle",
							"ttl_in_seconds": 120,
							"validity_in_seconds": 600,
							"worker_name": "some-worker",
							"type": "cache",
							"identifier": "a:b,c:d",
							"size": 1024
						},
						{
							"id": "some-import-handle",
							"ttl_in_seconds": 82800,
							"validity_in_seconds": 86400,
							"worker_name": "some-worker",
							"type": "import",
							"identifier": "some-path@some-version",
							"size": 2048
						},
						{
							"id": "some-output-handle",
							"ttl_in_seconds": 82800,
							"validity_in_seconds": 86400,
							"worker_name": "some-other-worker",
							"type": "output",
							"identifier": "some-output",
							"size": 4096
						},
						{
							"id": "some-cow-handle",
							"ttl_in_seconds": 0,
							"validity_in_seconds": 0,
							"worker_name": "some-worker",
							"type": "copy",
							"identifier": "some-parent-volume-handle",
							"size": 8192
						}
					]`))
				})
			})

			Context("when getting all builds fails", func() {
				BeforeEach(func() {
					volumesDB.GetVolumesReturns(nil, errors.New("oh no!"))
				})

				It("returns 500 Internal Server Error", func() {
					Expect(response.StatusCode).To(Equal(http.StatusInternalServerError))
				})
			})
		})
	})
})
