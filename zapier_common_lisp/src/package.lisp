;;;; package.lisp

(defpackage #:zapier-triggers
  (:use #:cl)
  (:import-from #:jonathan
                #:to-json
                #:parse)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-thread)
  (:export #:start-server
           #:stop-server
           #:*config*
           ;; Queue functions
           #:enqueue-event
           #:start-workers
           #:stop-workers
           #:queue-stats
           #:queue-depth))

(defpackage #:zapier-triggers.config
  (:use #:cl)
  (:export #:get-config
           #:parse-database-url
           #:*database-url*
           #:*port*
           #:*worker-count*
           #:*environment*))

(defpackage #:zapier-triggers.db
  (:use #:cl)
  (:import-from #:postmodern
                #:connect-toplevel
                #:disconnect-toplevel
                #:query
                #:execute)
  (:export #:connect-db
           #:disconnect-db
           #:with-connection
           #:db-connected-p
           #:init-schema))

(defpackage #:zapier-triggers.models
  (:use #:cl)
  (:export #:generate-api-key
           #:create-organization
           #:find-organization-by-key
           #:valid-api-key-p
           #:create-event
           #:find-events
           #:count-events
           #:event-belongs-to-org-p
           #:update-event-status
           #:create-webhook
           #:find-webhook))

(defpackage #:zapier-triggers.middleware
  (:use #:cl)
  (:export #:auth-middleware
           #:rate-limit-middleware
           #:error-handler-middleware))

(defpackage #:zapier-triggers.routes
  (:use #:cl)
  (:export #:health-check-handler
           #:generate-key-handler
           #:get-key-info-handler
           #:create-event-handler
           #:get-inbox-handler
           #:acknowledge-event-handler
           #:config-webhook-handler))

(defpackage #:zapier-triggers.utils
  (:use #:cl)
  (:export #:validate-event
           #:validate-event-type
           #:validate-payload-size
           #:validate-tier
           #:validate-url
           #:parse-json-body
           #:make-json-response
           #:json-success-response
           #:json-error-response
           #:generate-api-key
           #:generate-uuid
           #:generate-event-id
           ;; Dedup cache functions
           #:dedup-cache-check
           #:dedup-cache-add
           #:dedup-cache-clear
           #:dedup-cache-stats))
