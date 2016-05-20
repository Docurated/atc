package leaserunner_test

import (
	"errors"
	"os"
	"time"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/pivotal-golang/clock/fakeclock"
	"github.com/pivotal-golang/lager/lagertest"
	"github.com/tedsuo/ifrit"
	"github.com/tedsuo/ifrit/ginkgomon"

	dbfakes "github.com/concourse/atc/db/fakes"
	. "github.com/concourse/atc/leaserunner"
	"github.com/concourse/atc/leaserunner/fakes"
)

var _ = Describe("Runner", func() {
	var (
		fakeDB    *fakes.FakeRunnerDB
		fakeTask  *fakes.FakeTask
		fakeClock *fakeclock.FakeClock
		fakeLease *dbfakes.FakeLease

		interval time.Duration

		process ifrit.Process
	)

	BeforeEach(func() {
		fakeDB = new(fakes.FakeRunnerDB)
		fakeTask = new(fakes.FakeTask)
		fakeLease = new(dbfakes.FakeLease)
		fakeClock = fakeclock.NewFakeClock(time.Unix(123, 456))

		interval = 100 * time.Millisecond
	})

	JustBeforeEach(func() {
		process = ginkgomon.Invoke(NewRunner(
			lagertest.NewTestLogger("test"),
			fakeTask,
			"some-task-name",
			fakeDB,
			fakeClock,
			interval,
		))
	})

	AfterEach(func() {
		process.Signal(os.Interrupt)
		Expect(<-process.Wait()).ToNot(HaveOccurred())
	})

	Context("when the interval elapses", func() {
		JustBeforeEach(func() {
			fakeClock.WaitForWatcherAndIncrement(interval)
		})

		It("calls to get a lease for cache invalidation", func() {
			Eventually(fakeDB.GetLeaseCallCount).Should(Equal(1))
			_, actualTaskName, actualInterval := fakeDB.GetLeaseArgsForCall(0)
			Expect(actualTaskName).To(Equal("some-task-name"))
			Expect(actualInterval).To(Equal(interval))
		})

		Context("when getting a lease succeeds", func() {
			BeforeEach(func() {
				fakeDB.GetLeaseReturns(fakeLease, true, nil)
			})

			It("it collects lost baggage", func() {
				Eventually(fakeTask.RunCallCount).Should(Equal(1))
			})

			It("breaks the lease", func() {
				Eventually(fakeLease.BreakCallCount).Should(Equal(1))
			})

			Context("when collecting fails", func() {
				BeforeEach(func() {
					fakeTask.RunReturns(errors.New("disaster"))
				})

				It("does not exit the process", func() {
					Consistently(process.Wait()).ShouldNot(Receive())
				})

				It("breaks the lease", func() {
					Eventually(fakeLease.BreakCallCount).Should(Equal(1))
				})
			})
		})

		Context("when getting a lease fails", func() {
			Context("because of an error", func() {
				BeforeEach(func() {
					fakeDB.GetLeaseReturns(nil, true, errors.New("disaster"))
				})

				It("does not exit and does not collect baggage", func() {
					Consistently(fakeTask.RunCallCount).Should(Equal(0))
					Consistently(process.Wait()).ShouldNot(Receive())
				})
			})

			Context("because we got leased of false", func() {
				BeforeEach(func() {
					fakeDB.GetLeaseReturns(nil, false, nil)
				})

				It("does not exit and does not collect baggage", func() {
					Consistently(fakeTask.RunCallCount).Should(Equal(0))
					Consistently(process.Wait()).ShouldNot(Receive())
				})
			})
		})
	})
})
