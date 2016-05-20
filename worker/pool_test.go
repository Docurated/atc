package worker_test

import (
	"errors"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
	. "github.com/concourse/atc/worker"
	"github.com/concourse/atc/worker/fakes"
	"github.com/pivotal-golang/lager/lagertest"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Pool", func() {
	var (
		logger       *lagertest.TestLogger
		fakeProvider *fakes.FakeWorkerProvider

		pool Client
	)

	BeforeEach(func() {
		logger = lagertest.NewTestLogger("test")
		fakeProvider = new(fakes.FakeWorkerProvider)

		pool = NewPool(fakeProvider)
	})

	Describe("GetWorker", func() {
		Context("when the call to lookup the worker returns an error", func() {
			BeforeEach(func() {
				fakeProvider.GetWorkerReturns(nil, false, errors.New("disaster"))
			})

			It("returns an error", func() {
				foundWorker, err := pool.GetWorker("some-worker")
				Expect(err).To(HaveOccurred())
				Expect(foundWorker).To(BeNil())
			})
		})

		Context("when the call to lookup the worker fails because the worker was not found", func() {
			BeforeEach(func() {
				fakeProvider.GetWorkerReturns(nil, false, nil)
			})

			It("returns an error indicating no workers were found", func() {
				foundWorker, err := pool.GetWorker("no-worker")
				Expect(err).To(HaveOccurred())
				Expect(err).To(Equal(ErrNoWorkers))
				Expect(foundWorker).To(BeNil())
			})
		})

		Context("when the lookup of the worker succeeds", func() {
			var fakeWorker *fakes.FakeWorker

			BeforeEach(func() {
				fakeWorker = new(fakes.FakeWorker)
				fakeProvider.GetWorkerReturns(fakeWorker, true, nil)
			})

			It("returns an error indicating no workers were found", func() {
				foundWorker, err := pool.GetWorker("some-worker")
				Expect(err).ToNot(HaveOccurred())
				Expect(fakeProvider.GetWorkerCallCount()).To(Equal(1))
				workerName := fakeProvider.GetWorkerArgsForCall(0)
				Expect(workerName).To(Equal("some-worker"))
				Expect(foundWorker).To(Equal(fakeWorker))
			})
		})
	})

	Describe("Satisfying", func() {
		var (
			spec WorkerSpec

			satisfyingErr    error
			satisfyingWorker Worker
			resourceTypes    atc.ResourceTypes
		)

		BeforeEach(func() {
			spec = WorkerSpec{
				Platform: "some-platform",
				Tags:     []string{"step", "tags"},
			}
			resourceTypes = atc.ResourceTypes{
				{
					Name:   "some-resource-type",
					Type:   "some-underlying-type",
					Source: atc.Source{"some": "source"},
				},
			}
		})

		JustBeforeEach(func() {
			satisfyingWorker, satisfyingErr = pool.Satisfying(spec, resourceTypes)
		})

		Context("with multiple workers", func() {
			var (
				workerA *fakes.FakeWorker
				workerB *fakes.FakeWorker
				workerC *fakes.FakeWorker
			)

			BeforeEach(func() {
				workerA = new(fakes.FakeWorker)
				workerB = new(fakes.FakeWorker)
				workerC = new(fakes.FakeWorker)

				workerA.SatisfyingReturns(workerA, nil)
				workerB.SatisfyingReturns(workerB, nil)
				workerC.SatisfyingReturns(nil, errors.New("nope"))

				fakeProvider.WorkersReturns([]Worker{workerA, workerB, workerC}, nil)
			})

			It("succeeds", func() {
				Expect(satisfyingErr).NotTo(HaveOccurred())
			})

			It("checks that the workers satisfy the given spec", func() {
				Expect(workerA.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes := workerA.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec))
				Expect(actualResourceTypes).To(Equal(resourceTypes))

				Expect(workerB.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes = workerB.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec))
				Expect(actualResourceTypes).To(Equal(resourceTypes))

				Expect(workerC.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes = workerC.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec))
				Expect(actualResourceTypes).To(Equal(resourceTypes))
			})

			It("returns a random worker satisfying the spec", func() {
				chosenCount := map[Worker]int{workerA: 0, workerB: 0, workerC: 0}
				for i := 0; i < 100; i++ {
					satisfyingWorker, satisfyingErr = pool.Satisfying(spec, resourceTypes)
					Expect(satisfyingErr).NotTo(HaveOccurred())
					chosenCount[satisfyingWorker]++
				}
				Expect(chosenCount[workerA]).To(BeNumerically("~", chosenCount[workerB], 50))
				Expect(chosenCount[workerC]).To(BeZero())
			})

			Context("when no workers satisfy the spec", func() {
				BeforeEach(func() {
					workerA.SatisfyingReturns(nil, errors.New("nope"))
					workerB.SatisfyingReturns(nil, errors.New("nope"))
					workerC.SatisfyingReturns(nil, errors.New("nope"))
				})

				It("returns a NoCompatibleWorkersError", func() {
					Expect(satisfyingErr).To(Equal(NoCompatibleWorkersError{
						Spec:    spec,
						Workers: []Worker{workerA, workerB, workerC},
					}))
				})
			})
		})

		Context("with no workers", func() {
			BeforeEach(func() {
				fakeProvider.WorkersReturns([]Worker{}, nil)
			})

			It("returns ErrNoWorkers", func() {
				Expect(satisfyingErr).To(Equal(ErrNoWorkers))
			})
		})

		Context("when getting the workers fails", func() {
			disaster := errors.New("nope")

			BeforeEach(func() {
				fakeProvider.WorkersReturns(nil, disaster)
			})

			It("returns the error", func() {
				Expect(satisfyingErr).To(Equal(disaster))
			})
		})
	})

	Describe("AllSatisfying", func() {
		var (
			spec WorkerSpec

			satisfyingErr     error
			satisfyingWorkers []Worker
			resourceTypes     atc.ResourceTypes
		)

		BeforeEach(func() {
			spec = WorkerSpec{
				Platform: "some-platform",
				Tags:     []string{"step", "tags"},
			}
			resourceTypes = atc.ResourceTypes{
				{
					Name:   "some-resource-type",
					Type:   "some-underlying-type",
					Source: atc.Source{"some": "source"},
				},
			}
		})

		JustBeforeEach(func() {
			satisfyingWorkers, satisfyingErr = pool.AllSatisfying(spec, resourceTypes)
		})

		Context("with multiple workers", func() {
			var (
				workerA *fakes.FakeWorker
				workerB *fakes.FakeWorker
				workerC *fakes.FakeWorker
			)

			BeforeEach(func() {
				workerA = new(fakes.FakeWorker)
				workerB = new(fakes.FakeWorker)
				workerC = new(fakes.FakeWorker)

				workerA.SatisfyingReturns(workerA, nil)
				workerB.SatisfyingReturns(workerB, nil)
				workerC.SatisfyingReturns(nil, errors.New("nope"))

				fakeProvider.WorkersReturns([]Worker{workerA, workerB, workerC}, nil)
			})

			It("succeeds", func() {
				Expect(satisfyingErr).NotTo(HaveOccurred())
			})

			It("checks that the workers satisfy the given spec", func() {
				Expect(workerA.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes := workerA.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec))
				Expect(actualResourceTypes).To(Equal(resourceTypes))

				Expect(workerB.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes = workerB.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec))
				Expect(actualResourceTypes).To(Equal(resourceTypes))

				Expect(workerC.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes = workerC.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec))
				Expect(actualResourceTypes).To(Equal(resourceTypes))
			})

			It("returns all workers satisfying the spec in a random order", func() {
				firstCount := map[Worker]int{workerA: 0, workerB: 0}
				for i := 0; i < 100; i++ {
					satisfyingWorkers, satisfyingErr = pool.AllSatisfying(spec, resourceTypes)
					Expect(satisfyingErr).NotTo(HaveOccurred())
					Expect(satisfyingWorkers).To(ConsistOf(workerA, workerB))
					firstCount[satisfyingWorkers[0]]++
				}
				Expect(firstCount[workerA]).To(BeNumerically("~", firstCount[workerB], 50))
			})

			Context("when no workers satisfy the spec", func() {
				BeforeEach(func() {
					workerA.SatisfyingReturns(nil, errors.New("nope"))
					workerB.SatisfyingReturns(nil, errors.New("nope"))
					workerC.SatisfyingReturns(nil, errors.New("nope"))
				})

				It("returns a NoCompatibleWorkersError", func() {
					Expect(satisfyingErr).To(Equal(NoCompatibleWorkersError{
						Spec:    spec,
						Workers: []Worker{workerA, workerB, workerC},
					}))
				})
			})
		})

		Context("with no workers", func() {
			BeforeEach(func() {
				fakeProvider.WorkersReturns([]Worker{}, nil)
			})

			It("returns ErrNoWorkers", func() {
				Expect(satisfyingErr).To(Equal(ErrNoWorkers))
			})
		})

		Context("when getting the workers fails", func() {
			disaster := errors.New("nope")

			BeforeEach(func() {
				fakeProvider.WorkersReturns(nil, disaster)
			})

			It("returns the error", func() {
				Expect(satisfyingErr).To(Equal(disaster))
			})
		})
	})

	Describe("CreateContainer", func() {
		var (
			fakeImageFetchingDelegate *fakes.FakeImageFetchingDelegate

			id   Identifier
			spec ContainerSpec

			createdContainer Container
			createErr        error
			resourceTypes    atc.ResourceTypes
		)

		BeforeEach(func() {
			fakeImageFetchingDelegate = new(fakes.FakeImageFetchingDelegate)
			id = Identifier{
				ResourceID: 1234,
			}
			spec = ContainerSpec{ImageSpec: ImageSpec{ResourceType: "some-type"}}
			resourceTypes = atc.ResourceTypes{
				{
					Name:   "custom-type-b",
					Type:   "custom-type-a",
					Source: atc.Source{"some": "source"},
				},
				{
					Name:   "custom-type-a",
					Type:   "some-resource",
					Source: atc.Source{"some": "source"},
				},
				{
					Name:   "custom-type-c",
					Type:   "custom-type-b",
					Source: atc.Source{"some": "source"},
				},
				{
					Name:   "custom-type-d",
					Type:   "custom-type-b",
					Source: atc.Source{"some": "source"},
				},
				{
					Name:   "unknown-custom-type",
					Type:   "unknown-base-type",
					Source: atc.Source{"some": "source"},
				},
			}
		})

		JustBeforeEach(func() {
			createdContainer, createErr = pool.CreateContainer(logger, nil, fakeImageFetchingDelegate, id, Metadata{}, spec, resourceTypes)
		})

		Context("with multiple workers", func() {
			var (
				workerA *fakes.FakeWorker
				workerB *fakes.FakeWorker
				workerC *fakes.FakeWorker

				fakeContainer *fakes.FakeContainer
			)

			BeforeEach(func() {
				workerA = new(fakes.FakeWorker)
				workerB = new(fakes.FakeWorker)
				workerC = new(fakes.FakeWorker)

				workerA.ActiveContainersReturns(3)
				workerB.ActiveContainersReturns(2)

				workerA.SatisfyingReturns(workerA, nil)
				workerB.SatisfyingReturns(workerB, nil)
				workerC.SatisfyingReturns(nil, errors.New("nope"))

				fakeContainer = new(fakes.FakeContainer)
				workerA.CreateContainerReturns(fakeContainer, nil)
				workerB.CreateContainerReturns(fakeContainer, nil)
				workerC.CreateContainerReturns(fakeContainer, nil)

				fakeProvider.WorkersReturns([]Worker{workerA, workerB, workerC}, nil)
			})

			It("succeeds", func() {
				Expect(createErr).NotTo(HaveOccurred())
			})

			It("returns the created container", func() {
				Expect(createdContainer).To(Equal(fakeContainer))
			})

			It("checks that the workers satisfy the given spec", func() {
				Expect(workerA.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes := workerA.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec.WorkerSpec()))
				Expect(actualResourceTypes).To(Equal(resourceTypes))

				Expect(workerB.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes = workerB.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec.WorkerSpec()))
				Expect(actualResourceTypes).To(Equal(resourceTypes))

				Expect(workerC.SatisfyingCallCount()).To(Equal(1))
				actualSpec, actualResourceTypes = workerC.SatisfyingArgsForCall(0)
				Expect(actualSpec).To(Equal(spec.WorkerSpec()))
				Expect(actualResourceTypes).To(Equal(resourceTypes))
			})

			It("creates using a random worker", func() {
				for i := 1; i < 100; i++ { // account for initial create in JustBefore
					createdContainer, createErr := pool.CreateContainer(logger, nil, fakeImageFetchingDelegate, id, Metadata{}, spec, resourceTypes)
					Expect(createErr).NotTo(HaveOccurred())
					Expect(createdContainer).To(Equal(fakeContainer))
				}

				Expect(workerA.CreateContainerCallCount()).To(BeNumerically("~", workerB.CreateContainerCallCount(), 50))
				Expect(workerC.CreateContainerCallCount()).To(BeZero())
			})

			Context("when creating the container fails", func() {
				disaster := errors.New("nope")

				BeforeEach(func() {
					workerA.CreateContainerReturns(nil, disaster)
					workerB.CreateContainerReturns(nil, disaster)
				})

				It("returns the error", func() {
					Expect(createErr).To(Equal(disaster))
				})
			})

			Context("when no workers satisfy the spec", func() {
				BeforeEach(func() {
					workerA.SatisfyingReturns(nil, errors.New("nope"))
					workerB.SatisfyingReturns(nil, errors.New("nope"))
					workerC.SatisfyingReturns(nil, errors.New("nope"))
				})

				It("returns a NoCompatibleWorkersError", func() {
					Expect(createErr).To(Equal(NoCompatibleWorkersError{
						Spec:    spec.WorkerSpec(),
						Workers: []Worker{workerA, workerB, workerC},
					}))

				})
			})
		})

		Context("with no workers", func() {
			BeforeEach(func() {
				fakeProvider.WorkersReturns([]Worker{}, nil)
			})

			It("returns ErrNoWorkers", func() {
				Expect(createErr).To(Equal(ErrNoWorkers))
			})
		})

		Context("when getting the workers fails", func() {
			disaster := errors.New("nope")

			BeforeEach(func() {
				fakeProvider.WorkersReturns(nil, disaster)
			})

			It("returns the error", func() {
				Expect(createErr).To(Equal(disaster))
			})
		})
	})

	Describe("LookupContainer", func() {
		Context("when looking up the container info contains an error", func() {
			BeforeEach(func() {
				fakeProvider.GetContainerReturns(db.SavedContainer{}, false, errors.New("disaster"))
			})

			It("returns the error", func() {
				container, found, err := pool.LookupContainer(logger, "some-handle")
				Expect(err).To(HaveOccurred())
				Expect(container).To(BeNil())
				Expect(found).To(BeFalse())
			})
		})

		Context("when looking up the container info does not find the container info", func() {
			BeforeEach(func() {
				fakeProvider.GetContainerReturns(db.SavedContainer{}, false, nil)
			})

			It("returns that it was not found", func() {
				container, found, err := pool.LookupContainer(logger, "some-handle")
				Expect(err).NotTo(HaveOccurred())
				Expect(container).To(BeNil())
				Expect(found).To(BeFalse())
			})
		})

		Context("when looking up the container info is successful", func() {
			var container db.SavedContainer
			BeforeEach(func() {
				container = db.SavedContainer{
					Container: db.Container{
						ContainerMetadata: db.ContainerMetadata{
							WorkerName: "some-worker",
							Handle:     "some-container-handle",
						},
					},
				}

				fakeProvider.GetContainerReturns(container, true, nil)
			})

			It("calls to lookup the worker by name", func() {
				pool.LookupContainer(logger, "some-container-handle")

				Expect(fakeProvider.GetWorkerCallCount()).To(Equal(1))

				workerName := fakeProvider.GetWorkerArgsForCall(0)
				Expect(workerName).To(Equal("some-worker"))
			})

			Context("when looking up the worker returns an error", func() {
				BeforeEach(func() {
					fakeProvider.GetWorkerReturns(nil, false, errors.New("disaster"))
				})

				It("returns the error", func() {
					container, found, err := pool.LookupContainer(logger, "some-handle")
					Expect(err).To(HaveOccurred())
					Expect(container).To(BeNil())
					Expect(found).To(BeFalse())
				})
			})

			Context("when we cannot find the worker from the container info", func() {
				BeforeEach(func() {
					fakeProvider.GetWorkerReturns(nil, false, nil)
				})

				It("returns ErrMissingWorker", func() {
					container, found, err := pool.LookupContainer(logger, "some-handle")
					Expect(err).To(Equal(ErrMissingWorker))
					Expect(container).To(BeNil())
					Expect(found).To(BeFalse())
				})
			})

			Context("when looking up the worker is successful", func() {
				var fakeWorker *fakes.FakeWorker

				BeforeEach(func() {
					fakeWorker = new(fakes.FakeWorker)
					fakeProvider.GetWorkerReturns(fakeWorker, true, nil)
				})

				It("calls to lookup the container on the worker", func() {
					pool.LookupContainer(logger, "some-handle")

					Expect(fakeWorker.LookupContainerCallCount()).To(Equal(1))

					_, handleArg := fakeWorker.LookupContainerArgsForCall(0)
					Expect(handleArg).To(Equal("some-handle"))
				})

				Context("when looking up the container contains an error", func() {
					It("returns the error", func() {
						fakeWorker.LookupContainerReturns(nil, false, errors.New("disaster"))

						container, found, err := pool.LookupContainer(logger, "some-handle")
						Expect(err).To(HaveOccurred())
						Expect(container).To(BeNil())
						Expect(found).To(BeFalse())
					})
				})

				Context("when the container cannot be found on the worker", func() {
					BeforeEach(func() {
						fakeWorker.LookupContainerReturns(nil, false, nil)
					})

					It("expires the container and returns false and no error", func() {
						_, found, err := pool.LookupContainer(logger, "some-handle")
						Expect(err).ToNot(HaveOccurred())
						Expect(found).To(BeFalse())

						Expect(fakeProvider.ReapContainerCallCount()).To(Equal(1))

						expiredHandle := fakeProvider.ReapContainerArgsForCall(0)
						Expect(expiredHandle).To(Equal("some-handle"))
					})

					Context("when expiring the container fails", func() {
						disaster := errors.New("nope")

						BeforeEach(func() {
							fakeProvider.ReapContainerReturns(disaster)
						})

						It("returns the error", func() {
							_, _, err := pool.LookupContainer(logger, "some-handle")
							Expect(err).To(Equal(disaster))
						})
					})
				})

				Context("when the finding the container on the worker is successful", func() {
					It("returns the container", func() {
						var fakeContainer *fakes.FakeContainer
						fakeContainer = new(fakes.FakeContainer)

						fakeWorker.LookupContainerReturns(fakeContainer, true, nil)

						foundContainer, found, err := pool.LookupContainer(logger, "some-handle")
						Expect(err).NotTo(HaveOccurred())
						Expect(found).To(BeTrue())
						Expect(foundContainer).To(Equal(fakeContainer))
					})
				})
			})
		})
	})

	Describe("FindContainerForIdentifier", func() {
		var identifier Identifier

		BeforeEach(func() {
			identifier = Identifier{
				ResourceID: 1234,
			}
		})

		Context("when looking up the container info contains an error", func() {
			BeforeEach(func() {
				fakeProvider.FindContainerForIdentifierReturns(db.SavedContainer{}, false, errors.New("disaster"))
			})

			It("returns the error", func() {
				container, found, err := pool.FindContainerForIdentifier(logger, identifier)
				Expect(err).To(HaveOccurred())
				Expect(container).To(BeNil())
				Expect(found).To(BeFalse())
			})
		})

		Context("when looking up the container info does not find the container info", func() {
			BeforeEach(func() {
				fakeProvider.FindContainerForIdentifierReturns(db.SavedContainer{}, false, nil)
			})

			It("returns that it was not found", func() {
				container, found, err := pool.FindContainerForIdentifier(logger, identifier)
				Expect(err).NotTo(HaveOccurred())
				Expect(container).To(BeNil())
				Expect(found).To(BeFalse())
			})
		})

		Context("when looking up the container info is successful", func() {
			var container db.SavedContainer
			BeforeEach(func() {
				container = db.SavedContainer{
					Container: db.Container{
						ContainerMetadata: db.ContainerMetadata{
							WorkerName: "some-worker",
							Handle:     "some-container-handle",
						},
					},
				}

				fakeProvider.FindContainerForIdentifierReturns(container, true, nil)
			})

			It("calls to lookup the worker by name", func() {
				pool.FindContainerForIdentifier(logger, identifier)

				Expect(fakeProvider.GetWorkerCallCount()).To(Equal(1))

				workerName := fakeProvider.GetWorkerArgsForCall(0)
				Expect(workerName).To(Equal("some-worker"))
			})

			Context("when looking up the worker returns an error", func() {
				It("returns the error", func() {
					fakeProvider.GetWorkerReturns(nil, false, errors.New("disaster"))

					container, found, err := pool.FindContainerForIdentifier(logger, identifier)
					Expect(err).To(HaveOccurred())
					Expect(container).To(BeNil())
					Expect(found).To(BeFalse())
				})
			})

			Context("when we cannot find the worker from the container info", func() {
				BeforeEach(func() {
					fakeProvider.GetWorkerReturns(nil, false, nil)
				})

				It("returns ErrMissingWorker", func() {
					container, found, err := pool.FindContainerForIdentifier(logger, identifier)
					Expect(err).To(Equal(ErrMissingWorker))
					Expect(container).To(BeNil())
					Expect(found).To(BeFalse())
				})
			})

			Context("when looking up the worker is successful", func() {
				var fakeWorker *fakes.FakeWorker

				BeforeEach(func() {
					fakeWorker = new(fakes.FakeWorker)
					fakeProvider.GetWorkerReturns(fakeWorker, true, nil)
				})

				It("calls to lookup the container on the worker", func() {
					pool.FindContainerForIdentifier(logger, identifier)

					Expect(fakeWorker.LookupContainerCallCount()).To(Equal(1))

					_, handleArg := fakeWorker.LookupContainerArgsForCall(0)
					Expect(handleArg).To(Equal("some-container-handle"))
				})

				Context("when looking up the container contains an error", func() {
					It("returns the error", func() {
						fakeWorker.LookupContainerReturns(nil, false, errors.New("disaster"))

						container, found, err := pool.FindContainerForIdentifier(logger, identifier)
						Expect(err).To(HaveOccurred())
						Expect(container).To(BeNil())
						Expect(found).To(BeFalse())
					})
				})

				Context("when the container cannot be found on the worker", func() {
					BeforeEach(func() {
						fakeWorker.LookupContainerReturns(nil, false, nil)
					})

					It("expires the container and returns false and no error", func() {
						_, found, err := pool.FindContainerForIdentifier(logger, identifier)
						Expect(err).ToNot(HaveOccurred())
						Expect(found).To(BeFalse())

						Expect(fakeProvider.ReapContainerCallCount()).To(Equal(1))

						expiredHandle := fakeProvider.ReapContainerArgsForCall(0)
						Expect(expiredHandle).To(Equal("some-container-handle"))
					})

					Context("when expiring the container fails", func() {
						disaster := errors.New("nope")

						BeforeEach(func() {
							fakeProvider.ReapContainerReturns(disaster)
						})

						It("returns the error", func() {
							_, _, err := pool.FindContainerForIdentifier(logger, identifier)
							Expect(err).To(Equal(disaster))
						})
					})
				})

				Context("when the finding the container on the worker is successful", func() {
					It("returns the container", func() {
						var fakeContainer *fakes.FakeContainer
						fakeContainer = new(fakes.FakeContainer)

						fakeWorker.LookupContainerReturns(fakeContainer, true, nil)

						foundContainer, found, err := pool.FindContainerForIdentifier(logger, identifier)
						Expect(err).NotTo(HaveOccurred())
						Expect(found).To(BeTrue())
						Expect(foundContainer).To(Equal(fakeContainer))
					})
				})
			})
		})
	})
})
