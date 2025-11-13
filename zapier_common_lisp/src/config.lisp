;;;; config.lisp

(in-package #:zapier-triggers.config)

(defun get-config (key)
  "Get configuration value by key - reads environment variables at runtime"
  (case key
    (:database-url
     (or (uiop:getenv "DATABASE_URL")
         "postgresql://postgres:postgres@localhost:5432/zapier_triggers"))
    (:port
     (parse-integer (or (uiop:getenv "PORT") "5001")))
    (:worker-count
     (parse-integer (or (uiop:getenv "WORKER_COUNT") "4")))
    (:environment
     (or (uiop:getenv "ENVIRONMENT") "development"))
    (:log-level
     (or (uiop:getenv "LOG_LEVEL") "info"))
    (otherwise nil)))

(defun parse-database-url (url)
  "Parse PostgreSQL URL into connection parameters"
  ;; Simple manual parsing for postgres://user:pass@host:port/database?params or postgresql://...
  (let* ((without-scheme (cond
                          ((ppcre:scan "^postgresql://" url) (subseq url 13))  ; Remove "postgresql://"
                          ((ppcre:scan "^postgres://" url) (subseq url 11))    ; Remove "postgres://"
                          (t url)))
         ;; Remove query parameters (everything after ?)
         (query-pos (position #\? without-scheme))
         (without-query (if query-pos
                            (subseq without-scheme 0 query-pos)
                            without-scheme))
         (at-pos (position #\@ without-query))
         (slash-pos (position #\/ without-query :start (or at-pos 0)))
         (colon-pos-auth (if at-pos (position #\: without-query :end at-pos) nil))
         (colon-pos-host (if at-pos
                             (position #\: without-query :start (1+ at-pos) :end slash-pos)
                             (position #\: without-query :end slash-pos)))
         (user (if (and at-pos colon-pos-auth)
                   (subseq without-query 0 colon-pos-auth)
                   (if at-pos (subseq without-query 0 at-pos) "postgres")))
         (password (if (and at-pos colon-pos-auth)
                       (subseq without-query (1+ colon-pos-auth) at-pos)
                       nil))
         (host-start (if at-pos (1+ at-pos) 0))
         (host-end (or colon-pos-host slash-pos (length without-query)))
         (host (subseq without-query host-start host-end))
         (port (if colon-pos-host
                   (parse-integer (subseq without-query (1+ colon-pos-host) slash-pos))
                   5432))
         (database (if slash-pos
                       (subseq without-query (1+ slash-pos))
                       "zapier_triggers")))
    (format t "~&[CONFIG DEBUG] user=~S password=~S~%" user password)
    (list :host host
          :port port
          :database database
          :user user
          :password password)))
