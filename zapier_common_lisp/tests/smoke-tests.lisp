;;;; smoke-tests.lisp - Basic smoke tests for Zapier Triggers API
;;;; Run these to verify the server is functioning correctly

(require :asdf)

;; Add system directories
(push #p"/usr/share/common-lisp/systems/" asdf:*central-registry*)
(push #p"/usr/share/common-lisp/source/" asdf:*central-registry*)

;; Load dependencies
(asdf:load-system :drakma)
(asdf:load-system :yason)
(asdf:load-system :cl-ppcre)

(defpackage :zapier-smoke-tests
  (:use :cl)
  (:export #:run-tests))

(in-package :zapier-smoke-tests)

(defvar *test-results* nil "List of test results")
(defvar *api-key* nil "API key for testing")
(defvar *base-url* "http://localhost:5001" "Base URL for API")

;; Test utilities
(defun test-passed (name)
  (format t "~&✅ PASS: ~a~%" name)
  (push (list :name name :result :pass) *test-results*))

(defun test-failed (name reason)
  (format t "~&❌ FAIL: ~a - ~a~%" name reason)
  (push (list :name name :result :fail :reason reason) *test-results*))

(defun http-get (path)
  "Make HTTP GET request"
  (multiple-value-bind (body status headers)
      (drakma:http-request (format nil "~a~a" *base-url* path)
                          :method :get)
    (values body status headers)))

(defun http-post (path data &optional api-key)
  "Make HTTP POST request with JSON data"
  (let ((headers (list (cons "Content-Type" "application/json"))))
    (when api-key
      (push (cons "x-api-key" api-key) headers))
    (multiple-value-bind (body status)
        (drakma:http-request (format nil "~a~a" *base-url* path)
                            :method :post
                            :content-type "application/json"
                            :additional-headers headers
                            :content data)
      (values body status))))

(defun parse-json (body)
  "Parse JSON response"
  (handler-case
      (yason:parse (flexi-streams:octets-to-string body :external-format :utf-8))
    (error (e)
      (format t "~&[ERROR] Failed to parse JSON: ~a~%" e)
      nil)))

;; Test 1: Health check
(defun test-health-check ()
  (handler-case
      (multiple-value-bind (body status) (http-get "/health")
        (let ((json (parse-json body)))
          (if (and (= status 200)
                   json
                   (string= (gethash "status" json) "ok"))
              (test-passed "Health check")
              (test-failed "Health check"
                          (format nil "Expected 200 with status=ok, got ~a" status)))))
    (error (e)
      (test-failed "Health check" (format nil "Exception: ~a" e)))))

;; Test 2: Cache stats
(defun test-cache-stats ()
  (handler-case
      (multiple-value-bind (body status) (http-get "/stats/cache")
        (let ((json (parse-json body)))
          (if (and (= status 200)
                   json
                   (gethash "size" json)
                   (gethash "max-size" json))
              (test-passed "Cache stats endpoint")
              (test-failed "Cache stats endpoint"
                          (format nil "Expected 200 with size/max-size, got ~a" status)))))
    (error (e)
      (test-failed "Cache stats endpoint" (format nil "Exception: ~a" e)))))

;; Test 3: Generate API key
(defun test-generate-api-key ()
  (handler-case
      (let ((data "{\"organization_name\":\"SmokeTestOrg\",\"tier\":\"free\"}"))
        (multiple-value-bind (body status) (http-post "/api/keys/generate" data)
          (let ((json (parse-json body)))
            (if (and (= status 200)
                     json
                     (gethash "api-key" json)
                     (cl-ppcre:scan "^sk_" (gethash "api-key" json)))
                (progn
                  (setf *api-key* (gethash "api-key" json))
                  (format t "~&   Generated API key: ~a~%" *api-key*)
                  (test-passed "Generate API key"))
                (test-failed "Generate API key"
                            (format nil "Expected 200 with api-key, got ~a" status))))))
    (error (e)
      (test-failed "Generate API key" (format nil "Exception: ~a" e)))))

;; Test 4: Create event without API key (should fail)
(defun test-create-event-no-auth ()
  (handler-case
      (let ((data "{\"type\":\"test.event\",\"payload\":{\"test\":true}}"))
        (multiple-value-bind (body status) (http-post "/api/events" data)
          (if (= status 401)
              (test-passed "Create event without API key (auth required)")
              (test-failed "Create event without API key"
                          (format nil "Expected 401, got ~a" status)))))
    (error (e)
      (test-failed "Create event without API key" (format nil "Exception: ~a" e)))))

;; Test 5: Create event with API key
(defun test-create-event ()
  (unless *api-key*
    (test-failed "Create event" "No API key available")
    (return-from test-create-event))

  (handler-case
      (let ((data (format nil "{\"type\":\"smoke.test\",\"payload\":{\"test\":true},\"dedup_id\":\"smoke-~a\"}"
                         (get-universal-time))))
        (multiple-value-bind (body status) (http-post "/api/events" data *api-key*)
          (let ((json (parse-json body)))
            (if (and (= status 200)
                     json
                     (string= (gethash "status" json) "accepted")
                     (gethash "event-id" json))
                (progn
                  (format t "~&   Created event: ~a~%" (gethash "event-id" json))
                  (test-passed "Create event"))
                (test-failed "Create event"
                            (format nil "Expected 200 with status=accepted, got ~a" status))))))
    (error (e)
      (test-failed "Create event" (format nil "Exception: ~a" e)))))

;; Test 6: Verify duplicate detection
(defun test-duplicate-detection ()
  (unless *api-key*
    (test-failed "Duplicate detection" "No API key available")
    (return-from test-duplicate-detection))

  (handler-case
      (let* ((dedup-id (format nil "dedup-test-~a" (get-universal-time)))
             (data (format nil "{\"type\":\"dedup.test\",\"payload\":{\"num\":1},\"dedup_id\":\"~a\"}"
                          dedup-id)))
        ;; Create first event
        (multiple-value-bind (body1 status1) (http-post "/api/events" data *api-key*)
          (let ((json1 (parse-json body1)))
            (if (not (string= (gethash "status" json1) "accepted"))
                (test-failed "Duplicate detection" "First event not accepted")
                ;; Try to create duplicate
                (progn
                  (sleep 0.1) ; Small delay
                  (multiple-value-bind (body2 status2) (http-post "/api/events" data *api-key*)
                    (let ((json2 (parse-json body2)))
                      (if (and (= status2 200)
                               (string= (gethash "status" json2) "duplicate"))
                          (test-passed "Duplicate detection")
                          (test-failed "Duplicate detection"
                                      (format nil "Expected duplicate status, got ~a"
                                             (gethash "status" json2)))))))))))
    (error (e)
      (test-failed "Duplicate detection" (format nil "Exception: ~a" e)))))

;; Test 7: Get inbox
(defun test-get-inbox ()
  (unless *api-key*
    (test-failed "Get inbox" "No API key available")
    (return-from test-get-inbox))

  (handler-case
      (multiple-value-bind (body status)
          (drakma:http-request (format nil "~a/api/inbox?limit=10" *base-url*)
                              :method :get
                              :additional-headers (list (cons "x-api-key" *api-key*)))
        (let ((json (parse-json body)))
          (if (and (= status 200)
                   json
                   (gethash "events" json)
                   (gethash "count" json))
              (progn
                (format t "~&   Retrieved ~a events from inbox~%" (gethash "count" json))
                (test-passed "Get inbox"))
              (test-failed "Get inbox"
                          (format nil "Expected 200 with events/count, got ~a" status)))))
    (error (e)
      (test-failed "Get inbox" (format nil "Exception: ~a" e)))))

;; Test 8: Invalid API key
(defun test-invalid-api-key ()
  (handler-case
      (let ((data "{\"type\":\"test.event\",\"payload\":{\"test\":true}}"))
        (multiple-value-bind (body status)
            (http-post "/api/events" data "sk_invalid_key_12345")
          (if (= status 401)
              (test-passed "Invalid API key rejected")
              (test-failed "Invalid API key rejected"
                          (format nil "Expected 401, got ~a" status)))))
    (error (e)
      (test-failed "Invalid API key rejected" (format nil "Exception: ~a" e)))))

;; Main test runner
(defun run-tests ()
  "Run all smoke tests"
  (setf *test-results* nil)
  (setf *api-key* nil)

  (format t "~%~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "   Zapier Triggers API - Smoke Tests~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "~%Running tests against: ~a~%~%" *base-url*)

  ;; Run all tests
  (test-health-check)
  (test-cache-stats)
  (test-generate-api-key)
  (test-create-event-no-auth)
  (test-create-event)
  (test-duplicate-detection)
  (test-get-inbox)
  (test-invalid-api-key)

  ;; Print summary
  (format t "~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "   Test Summary~%")
  (format t "═══════════════════════════════════════════════════════════~%")

  (let ((passed (count-if (lambda (r) (eq (getf r :result) :pass)) *test-results*))
        (failed (count-if (lambda (r) (eq (getf r :result) :fail)) *test-results*))
        (total (length *test-results*)))
    (format t "~%Total:  ~a tests~%" total)
    (format t "Passed: ~a (~a%)~%" passed (if (> total 0) (round (* 100 (/ passed total))) 0))
    (format t "Failed: ~a~%~%" failed)

    (when (> failed 0)
      (format t "Failed tests:~%")
      (dolist (result *test-results*)
        (when (eq (getf result :result) :fail)
          (format t "  - ~a: ~a~%" (getf result :name) (getf result :reason)))))

    (if (= failed 0)
        (format t "~%✅ All tests passed!~%~%")
        (format t "~%❌ Some tests failed~%~%"))

    (= failed 0)))

;; Auto-run when loaded
(format t "~%Smoke tests loaded. Run with: (zapier-smoke-tests:run-tests)~%")
