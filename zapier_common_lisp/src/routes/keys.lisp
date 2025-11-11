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
                 :|organization_name| org-name
                 :|tier| tier
                 :|created_at| (local-time:format-timestring
                               nil (local-time:now)))
           :status 201)
          (zapier-triggers.utils:json-error-response
           "Failed to generate API key"
           :status 500
           :code "generation_failed")))))

(defun get-key-info-handler (params)
  "Get API key info endpoint handler"
  (let* ((env (getf params :env))
         (org (getf env :organization)))
    (if org
        (zapier-triggers.utils:json-success-response
         (list :|api_key| (getf org :api-key)
               :|organization_name| (getf org :name)
               :|tier| (getf org :tier)
               :|created_at| (local-time:format-timestring
                             nil (getf org :created-at))))
        (zapier-triggers.utils:json-error-response
         "Organization not found"
         :status 404
         :code "not_found"))))
