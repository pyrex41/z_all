;;;; routes/keys.lisp

(in-package #:zapier-triggers.routes)

(defun generate-key-handler (params)
  "Generate API key endpoint handler"
  (let* ((body (getf params :body))
         (data (zapier-triggers.utils:parse-json-body body))
         (org-name (or (getf data :|organization_name|)
                       (getf data :organization-name)
                       "Default Organization"))
         (tier (or (getf data :|tier|)
                  (getf data :tier)
                  "free")))

    ;; Validate tier
    (unless (zapier-triggers.utils:validate-tier tier)
      (return-from generate-key-handler
        (zapier-triggers.utils:json-error-response
         "Invalid tier. Must be one of: free, starter, professional, enterprise"
         :status 400
         :code "invalid_tier")))

    ;; Create organization
    (let ((org (zapier-triggers.models:create-organization org-name :tier tier)))
      (if org
          (zapier-triggers.utils:json-success-response
           (list :|api_key| (getf org :api-key)
                 :|organization_id| (getf org :id)
                 :|organization_name| org-name
                 :|tier| tier
                 :|created_at| (local-time:format-timestring
                               nil (local-time:now)))
           :status 201)
          (zapier-triggers.utils:json-error-response
           "Failed to generate API key"
           :status 500
           :code "generation_failed")))))

(defun get-rate-limit-for-tier (tier)
  "Get rate limit per minute for tier"
  (cond
    ((string= tier "free") 10)
    ((string= tier "starter") 60)
    ((string= tier "professional") 600)
    ((string= tier "enterprise") 6000)
    (t 10)))

(defun get-key-info-handler (params)
  "Get API key info endpoint handler"
  (let* ((env (getf params :env))
         (org (getf env :organization)))
    (if org
        (let ((created-at (getf org :created-at))
              (tier (getf org :tier)))
          (zapier-triggers.utils:json-success-response
           (list :|api_key| (getf org :api-key)
                 :|organization_id| (getf org :id)
                 :|organization_name| (getf org :name)
                 :|tier| tier
                 :|rate_limit_per_minute| (get-rate-limit-for-tier tier)
                 :|created_at| (if (integerp created-at)
                                   (local-time:format-timestring
                                    nil
                                    (local-time:unix-to-timestamp created-at))
                                   (local-time:format-timestring
                                    nil created-at)))))
        (zapier-triggers.utils:json-error-response
         "Organization not found"
         :status 404
         :code "not_found"))))
