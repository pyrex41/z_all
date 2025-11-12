;;;; routes/inbox.lisp

(in-package #:zapier-triggers.routes)

(defun get-inbox-handler (params)
  "Get inbox endpoint handler"
  (let* ((env (getf params :env))
         (query-params (getf params :query-params))
         (org (getf env :organization))
         (org-id (getf org :id)))

    (unless org-id
      (return-from get-inbox-handler
        (zapier-triggers.utils:json-error-response
         "Organization not found"
         :status 401
         :code "unauthorized")))

    ;; Parse query parameters
    (let* ((status (cdr (assoc "status" query-params :test #'string=)))
           (limit-str (cdr (assoc "limit" query-params :test #'string=)))
           (offset-str (cdr (assoc "offset" query-params :test #'string=)))
           (limit (if limit-str
                     (min (parse-integer limit-str :junk-allowed t) 1000)
                     100))
           (offset (if offset-str
                      (parse-integer offset-str :junk-allowed t)
                      0)))

      ;; Validate status if provided
      (when (and status
                 (not (member status '("pending" "delivered" "failed") :test #'string=)))
        (return-from get-inbox-handler
          (zapier-triggers.utils:json-error-response
           "Invalid status. Must be one of: pending, delivered, failed"
           :status 400
           :code "invalid_status")))

      ;; Fetch events
      (let ((events (zapier-triggers.models:find-events
                     org-id
                     :status status
                     :limit limit
                     :offset offset))
            (total (zapier-triggers.models:count-events org-id status)))

        (zapier-triggers.utils:json-success-response
         (list :|events| (mapcar (lambda (event)
                                   (let ((created-at (getf event :created-at)))
                                     (list :|id| (getf event :id)
                                           :|type| (getf event :type)
                                           :|payload| (getf event :payload)
                                           :|status| (getf event :status)
                                           :|created_at| (if (integerp created-at)
                                                             (local-time:format-timestring
                                                              nil
                                                              (local-time:unix-to-timestamp created-at))
                                                             (local-time:format-timestring
                                                              nil created-at)))))
                                 events)
               :|count| (length events)
               :|total| total
               :|limit| limit
               :|offset| offset))))))
