;;;; models/organization.lisp

(in-package #:zapier-triggers.models)

(defun generate-api-key ()
  "Generate a new API key"
  (zapier-triggers.utils:generate-api-key))

(defun create-organization (name &key (tier "free"))
  "Create a new organization with API key"
  (let ((api-key (generate-api-key)))
    (zapier-triggers.db:with-connection
      (let ((org-id (zapier-triggers.db::insert-organization name api-key tier)))
        (list :id org-id
              :name name
              :api-key api-key
              :tier tier)))))

(defun find-organization-by-key (api-key)
  "Find organization by API key"
  (zapier-triggers.db:with-connection
    (let ((row (zapier-triggers.db::find-organization-by-api-key api-key)))
      (when row
        (destructuring-bind (id name key tier created-at) row
          (list :id id
                :name name
                :api-key key
                :tier tier
                :created-at created-at))))))

(defun valid-api-key-p (api-key)
  "Check if API key is valid"
  (and api-key
       (find-organization-by-key api-key)
       t))

(defun get-organization-tier (api-key)
  "Get tier for organization by API key"
  (let ((org (find-organization-by-key api-key)))
    (when org
      (getf org :tier))))
