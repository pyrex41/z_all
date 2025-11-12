;;;; db/connection.lisp

(in-package #:zapier-triggers.db)

(defvar *db-connection-pool* nil
  "Database connection pool")

(defvar *db-connection-lock* (bt:make-lock "db-connection-lock")
  "Lock for thread-safe database operations")

(defun parse-db-url (url)
  "Parse DATABASE_URL into Postmodern connection parameters"
  ;; Use the config parser and convert to Postmodern format
  (let* ((parsed (zapier-triggers.config:parse-database-url url))
         (database (getf parsed :database))
         (user (or (getf parsed :user) "postgres"))
         (password (or (getf parsed :password) "postgres"))
         (host (or (getf parsed :host) "localhost"))
         (port (or (getf parsed :port) 5432)))
    (list database user password host :port port)))

(defun connect-db ()
  "Establish pooled connection to PostgreSQL database"
  (bt:with-lock-held (*db-connection-lock*)
    (unless *db-connection-pool*
      (let* ((db-url (zapier-triggers.config:get-config :database-url))
             (conn-params (parse-db-url db-url)))
        (handler-case
            (progn
              (apply #'postmodern:connect-toplevel conn-params)
              (setf *db-connection-pool* t)
              (format t "~&[DB] Connected to database~%")
              t)
          (error (e)
            (format t "~&[DB ERROR] Failed to connect: ~A~%" e)
            nil))))))

(defun disconnect-db ()
  "Close database connection"
  (bt:with-lock-held (*db-connection-lock*)
    (when *db-connection-pool*
      (handler-case
          (progn
            (postmodern:disconnect-toplevel)
            (setf *db-connection-pool* nil)
            (format t "~&[DB] Disconnected from database~%")
            t)
        (error (e)
          (format t "~&[DB ERROR] Failed to disconnect: ~A~%" e)
          nil)))))

(defun db-connected-p ()
  "Check if database connection is active"
  (handler-case
      (progn
        (postmodern:query "SELECT 1" :single)
        t)
    (error ()
      nil)))

(defmacro with-connection (&body body)
  "Execute body with database connection"
  `(progn
     (unless *db-connection-pool*
       (connect-db))
     ,@body))

(defun split-sql-statements (sql)
  "Split SQL string into individual statements, handling semicolons in function bodies"
  (let ((statements '())
        (current "")
        (in-function nil))
    (loop for line in (cl-ppcre:split "\\n" sql)
          do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
               ;; Track if we're inside a function definition
               (when (cl-ppcre:scan "(?i)CREATE\\s+(OR\\s+REPLACE\\s+)?FUNCTION" trimmed)
                 (setf in-function t))
               (when (and in-function (cl-ppcre:scan "(?i)\\$\\$\\s*language" trimmed))
                 (setf in-function nil))

               ;; Append line to current statement
               (setf current (concatenate 'string current line (string #\Newline)))

               ;; If we hit a semicolon outside of a function, split
               (when (and (not in-function)
                         (cl-ppcre:scan ";\\s*$" trimmed)
                         (not (string= trimmed ""))
                         (not (cl-ppcre:scan "^--" trimmed)))
                 (push current statements)
                 (setf current ""))))
    (nreverse statements)))

(defun init-schema ()
  "Initialize database schema from SQL file"
  (with-connection
    (let ((schema-path (asdf:system-relative-pathname
                        :zapier-triggers "sql/schema.sql")))
      (when (probe-file schema-path)
        (let* ((schema-sql (uiop:read-file-string schema-path))
               (statements (split-sql-statements schema-sql))
               (success-count 0))
          (handler-case
              (progn
                (dolist (stmt statements)
                  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) stmt)))
                    (unless (or (string= trimmed "")
                               (cl-ppcre:scan "^--" trimmed))
                      (handler-case
                          (progn
                            (postmodern:execute trimmed)
                            (incf success-count))
                        (error (e)
                          (format t "~&[DB WARNING] Statement failed (may be expected): ~A~%" e))))))
                (format t "~&[DB] Schema initialized successfully (~D statements executed)~%"
                        success-count)
                t)
            (error (e)
              (format t "~&[DB ERROR] Failed to initialize schema: ~A~%" e)
              nil)))))))
