;;;; models/webhook.lisp

(in-package #:zapier-triggers.models)

(defun create-webhook (org-id url &key auth-headers retry-count timeout)
  "Create or update webhook configuration"
  (let ((auth-json (when auth-headers
                     (jonathan:to-json auth-headers :from :plist))))
    (zapier-triggers.db:with-connection
      (handler-case
          (let ((webhook-id (zapier-triggers.db::insert-webhook
                             org-id url
                             :auth-headers auth-json
                             :retry-count retry-count
                             :timeout timeout)))
            (list :id webhook-id
                  :organization-id org-id
                  :url url
                  :retry-count (or retry-count 3)
                  :timeout (or timeout 30)))
        (error (e)
          (format t "~&[WEBHOOK ERROR] Failed to create webhook: ~A~%" e)
          nil)))))

(defun find-webhook (org-id)
  "Find webhook configuration for organization"
  (zapier-triggers.db:with-connection
    (let ((row (zapier-triggers.db::find-webhook-by-organization org-id)))
      (when row
        (destructuring-bind (id org-id url auth-json retry timeout created) row
          (list :id id
                :organization-id org-id
                :url url
                :auth-headers (when auth-json
                                (jonathan:parse auth-json :as :plist))
                :retry-count retry
                :timeout timeout
                :created-at created))))))
