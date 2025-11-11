;;;; config.lisp

(in-package #:zapier-triggers.config)

(defparameter *database-url*
  (or (uiop:getenv "DATABASE_URL")
      "postgresql://postgres:postgres@localhost:5432/zapier_triggers")
  "PostgreSQL database connection URL")

(defparameter *port*
  (parse-integer (or (uiop:getenv "PORT") "5000"))
  "HTTP server port")

(defparameter *worker-count*
  (parse-integer (or (uiop:getenv "WORKER_COUNT") "4"))
  "Number of Woo worker processes")

(defparameter *environment*
  (or (uiop:getenv "ENVIRONMENT") "development")
  "Runtime environment (development, production)")

(defparameter *log-level*
  (or (uiop:getenv "LOG_LEVEL") "info")
  "Logging level")

(defun get-config (key)
  "Get configuration value by key"
  (case key
    (:database-url *database-url*)
    (:port *port*)
    (:worker-count *worker-count*)
    (:environment *environment*)
    (:log-level *log-level*)
    (otherwise nil)))

(defun parse-database-url (url)
  "Parse PostgreSQL URL into connection parameters"
  ;; Simple manual parsing for postgresql://user:pass@host:port/database
  (let* ((without-scheme (if (ppcre:scan "^postgresql://" url)
                              (subseq url 13)  ; Remove "postgresql://"
                              url))
         (at-pos (position #\@ without-scheme))
         (slash-pos (position #\/ without-scheme :start (or at-pos 0)))
         (colon-pos-auth (if at-pos (position #\: without-scheme :end at-pos) nil))
         (colon-pos-host (if at-pos
                             (position #\: without-scheme :start (1+ at-pos) :end slash-pos)
                             (position #\: without-scheme :end slash-pos)))
         (user (if (and at-pos colon-pos-auth)
                   (subseq without-scheme 0 colon-pos-auth)
                   (if at-pos (subseq without-scheme 0 at-pos) "postgres")))
         (password (if (and at-pos colon-pos-auth)
                       (subseq without-scheme (1+ colon-pos-auth) at-pos)
                       nil))
         (host-start (if at-pos (1+ at-pos) 0))
         (host-end (or colon-pos-host slash-pos (length without-scheme)))
         (host (subseq without-scheme host-start host-end))
         (port (if colon-pos-host
                   (parse-integer (subseq without-scheme (1+ colon-pos-host) slash-pos))
                   5432))
         (database (if slash-pos
                       (subseq without-scheme (1+ slash-pos))
                       "zapier_triggers")))
    (list :host host
          :port port
          :database database
          :user user
          :password password)))
