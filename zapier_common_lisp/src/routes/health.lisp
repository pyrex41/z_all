;;;; routes/health.lisp

(in-package #:zapier-triggers.routes)

(defun health-check-handler (params)
  "Health check endpoint handler"
  (declare (ignore params))
  (let ((db-ok (zapier-triggers.db:db-connected-p))
        (timestamp (local-time:now)))
    (zapier-triggers.utils:make-json-response
     (list :status (if db-ok "ok" "degraded")
           :database db-ok
           :timestamp (local-time:format-timestring nil timestamp))
     :status (if db-ok 200 503))))
