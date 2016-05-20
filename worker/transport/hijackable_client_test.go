package transport_test

import (
	"errors"
	"net/http"
	"net/url"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/concourse/atc/db"
	"github.com/concourse/atc/worker/transport"
	"github.com/concourse/atc/worker/transport/fakes"
	"github.com/concourse/retryhttp"
	retryhttpfakes "github.com/concourse/retryhttp/fakes"
)

var _ = Describe("HijackableClient #Do", func() {
	var (
		request              http.Request
		fakeDB               *fakes.FakeTransportDB
		savedWorker          db.SavedWorker
		fakeHijackableClient *retryhttpfakes.FakeHijackableClient
		hijackableClient     retryhttp.HijackableClient
		response             *http.Response
		err                  error
		fakeHijackCloser     *retryhttpfakes.FakeHijackCloser
		actualHijackCloser   retryhttp.HijackCloser
	)

	BeforeEach(func() {
		fakeDB = new(fakes.FakeTransportDB)
		fakeHijackableClient = new(retryhttpfakes.FakeHijackableClient)
		fakeHijackCloser = new(retryhttpfakes.FakeHijackCloser)
		hijackableClient = transport.NewHijackableClient("some-worker", fakeDB, fakeHijackableClient)
		requestUrl, err := url.Parse("http://1.2.3.4/something")
		Expect(err).NotTo(HaveOccurred())

		request = http.Request{
			URL: requestUrl,
		}

		savedWorker = db.SavedWorker{
			WorkerInfo: db.WorkerInfo{
				GardenAddr: "some-garden-addr",
			},
			ExpiresIn: 123,
		}

		fakeDB.GetWorkerReturns(savedWorker, true, nil)

		fakeHijackableClient.DoReturns(&http.Response{StatusCode: http.StatusTeapot}, fakeHijackCloser, nil)
	})

	JustBeforeEach(func() {
		response, actualHijackCloser, err = hijackableClient.Do(&request)
	})

	It("returns the response", func() {
		Expect(err).NotTo(HaveOccurred())
		Expect(actualHijackCloser).To(Equal(fakeHijackCloser))
		Expect(response).To(Equal(&http.Response{StatusCode: http.StatusTeapot}))
	})

	It("sends the request with worker's garden address", func() {
		Expect(fakeHijackableClient.DoCallCount()).To(Equal(1))
		actualRequest := fakeHijackableClient.DoArgsForCall(0)
		Expect(actualRequest.URL.Host).To(Equal(savedWorker.GardenAddr))
		Expect(actualRequest.URL.Path).To(Equal("/something"))
	})

	Context("when the lookup of the worker in the db errors", func() {
		var expectedErr error
		BeforeEach(func() {
			expectedErr = errors.New("some-db-error")
			fakeDB.GetWorkerReturns(db.SavedWorker{}, true, expectedErr)
		})

		It("throws an error", func() {
			Expect(err).To(HaveOccurred())
			Expect(err.Error()).To(ContainSubstring(expectedErr.Error()))
		})
	})

	Context("when the worker is not found in the db", func() {
		BeforeEach(func() {
			fakeDB.GetWorkerReturns(db.SavedWorker{}, false, nil)
		})

		It("throws an error", func() {
			Expect(err).To(HaveOccurred())
			Expect(err).To(Equal(transport.ErrMissingWorker{WorkerName: "some-worker"}))
		})
	})

	It("reuses the request cached host on subsequent calls", func() {
		Expect(fakeDB.GetWorkerCallCount()).To(Equal(1))
		_, _, err := hijackableClient.Do(&request)
		Expect(err).NotTo(HaveOccurred())
		Expect(fakeDB.GetWorkerCallCount()).To(Equal(1))
	})

	Context("when inner Do fails", func() {
		BeforeEach(func() {
			fakeHijackableClient.DoReturns(nil, nil, errors.New("some-error"))
		})

		It("updates cached request host", func() {
			Expect(fakeDB.GetWorkerCallCount()).To(Equal(1))
			_, _, err := hijackableClient.Do(&request)
			Expect(err).To(HaveOccurred())
			Expect(fakeDB.GetWorkerCallCount()).To(Equal(2))
		})
	})
})
