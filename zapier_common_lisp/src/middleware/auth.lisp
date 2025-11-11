;;;; middleware/auth.lisp

(in-package #:zapier-triggers.middleware)

(defun get-header (env header-name)
  "Get header value from environment"
  (gethash header-name (getf env :headers)))

(defun auth-middleware (app)
  "Middleware to authenticate requests via API key"
  (lambda (env)
    (let ((path (getf env :path-info)))
      ;; Skip authentication for health check and key generation
      (if (or (string= path "/health")
              (string= path "/api/keys/generate"))
          (funcall app env)
          ;; Require authentication for other endpoints
          (let ((api-key (get-header env "x-api-key")))
            (if (and api-key
                     (zapier-triggers.models:valid-api-key-p api-key))
                (let ((org (zapier-triggers.models:find-organization-by-key api-key)))
                  ;; Add organization context to environment
                  (funcall app (append env (list :organization org))))
                ;; Unauthorized
                (zapier-triggers.utils:json-error-response
                 "Invalid or missing API key"
                 :status 401
                 :code "unauthorized")))))))
