;;;; routes/webhook.lisp

(in-package #:zapier-triggers.routes)

(defun config-webhook-handler (params)
  "Configure webhook endpoint handler"
  (let* ((env (getf params :env))
         (body (getf params :body))
         (org (getf env :organization))
         (org-id (getf org :id)))

    (unless org-id
      (return-from config-webhook-handler
        (zapier-triggers.utils:json-error-response
         "Organization not found"
         :status 401
         :code "unauthorized")))

    ;; Parse request body
    (let ((data (zapier-triggers.utils:parse-json-body body)))
      (unless data
        (return-from config-webhook-handler
          (zapier-triggers.utils:json-error-response
           "Invalid JSON"
           :status 400
           :code "invalid_json")))

      (let ((url (or (getf data :|url|) (getf data :url)))
            (auth-headers (or (getf data :|auth_headers|)
                             (getf data :auth-headers)))
            (retry-count (or (getf data :|retry_count|)
                            (getf data :retry-count)))
            (timeout (or (getf data :|timeout|) (getf data :timeout))))

        ;; Validate URL
        (unless (zapier-triggers.utils:validate-url url)
          (return-from config-webhook-handler
            (zapier-triggers.utils:json-error-response
             "Invalid or missing URL"
             :status 400
             :code "invalid_url")))

        ;; Create webhook
        (let ((webhook (zapier-triggers.models:create-webhook
                        org-id url
                        :auth-headers auth-headers
                        :retry-count retry-count
                        :timeout timeout)))
          (if webhook
              (zapier-triggers.utils:json-success-response
               (list :|id| (getf webhook :id)
                     :|url| url
                     :|retry_count| (getf webhook :retry-count)
                     :|timeout| (getf webhook :timeout))
               :status 201)
              (zapier-triggers.utils:json-error-response
               "Failed to configure webhook"
               :status 500
               :code "config_failed")))))))
