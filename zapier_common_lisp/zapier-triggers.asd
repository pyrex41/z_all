;;;; zapier-triggers.asd

(asdf:defsystem #:zapier-triggers
  :description "Zapier Triggers API - Common Lisp implementation using Woo"
  :author "Zapier Development Team"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:woo
               #:clack
               #:lack
               #:flexi-streams
               #:postmodern
               #:jonathan
               #:local-time
               #:uuid
               #:cl-ppcre
               #:bordeaux-threads
               #:trivial-backtrace
               #:log4cl
               #:ironclad
               #:puri
               #:lparallel
               #:chanl)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "config")
                 (:module "utils"
                  :components
                  ((:file "json")
                   (:file "validation")
                   (:file "crypto")
                   (:file "dedup-cache")))
                 (:module "db"
                  :components
                  ((:file "connection")
                   (:file "queries")))
                 (:module "models"
                  :components
                  ((:file "organization")
                   (:file "event")
                   (:file "webhook")))
                 (:module "middleware"
                  :components
                  ((:file "auth")
                   (:file "rate-limit")
                   (:file "error-handler")))
                 (:module "workers"
                  :components
                  ((:file "queue")))
                 (:module "routes"
                  :components
                  ((:file "health")
                   (:file "keys")
                   (:file "events")
                   (:file "inbox")
                   (:file "webhook")))
                 (:file "server"))))
  :in-order-to ((test-op (test-op #:zapier-triggers/tests))))

(asdf:defsystem #:zapier-triggers/tests
  :description "Test suite for Zapier Triggers API"
  :author "Zapier Development Team"
  :license "MIT"
  :depends-on (#:zapier-triggers
               #:fiveam
               #:drakma)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "test-suite")
                 (:module "unit"
                  :components
                  ((:file "test-events")
                   (:file "test-keys")))
                 (:module "integration"
                  :components
                  ((:file "test-api"))))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :zapier-triggers-suite)))
