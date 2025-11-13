;;;; db/connection.lisp

(in-package #:zapier-triggers.db)

(defvar *db-connection-pool* nil
  "Database connection pool flag")

(defvar *db-connection-lock* (bt:make-lock "db-connection-lock")
  "Lock for thread-safe database operations")

(defun parse-db-url (url)
  "Parse DATABASE_URL into Postmodern connection parameters"
  ;; Use the config parser and convert to Postmodern format
  (format t "~&[DB DEBUG] Parsing DATABASE_URL: ~A~%" url)
  (let* ((parsed (zapier-triggers.config:parse-database-url url))
         (database (getf parsed :database))
         (user (or (getf parsed :user) "postgres"))
         (password (or (getf parsed :password) "postgres"))
         (host (or (getf parsed :host) "localhost"))
         (port (or (getf parsed :port) 5432)))
    (format t "~&[DB DEBUG] Parsed - host: ~A, port: ~A, database: ~A, user: ~A~%" host port database user)
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
  "Execute body with database connection - creates new connection per request for thread safety.
   Postmodern's connect-toplevel creates global connection which causes 'still processing' errors
   under concurrent load. Each request gets its own connection for safety."
  `(let* ((db-url (zapier-triggers.config:get-config :database-url))
          (conn-params (parse-db-url db-url)))
     (postmodern:with-connection conn-params
       ,@body)))

(defun split-sql-statements (sql)
  "Split SQL string into individual statements, handling dollar-quoted strings in functions"
  (let ((statements '())
        (current "")
        (in-dollar-quote nil))
    (loop for line in (cl-ppcre:split "\\n" sql)
          do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
               ;; Append line to current statement
               (setf current (concatenate 'string current line (string #\Newline)))

               ;; Toggle dollar-quote state when we see $$
               (when (cl-ppcre:scan "\\$\\$" trimmed)
                 (setf in-dollar-quote (not in-dollar-quote)))

               ;; Split on semicolon only if not inside dollar quotes
               ;; and not a comment
               (when (and (not in-dollar-quote)
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
               (success-count 0)
               (skip-count 0))
          (format t "~&[DB DEBUG] Found ~D statements in schema~%" (length statements))
          (handler-case
              (progn
                (loop for stmt in statements
                      for idx from 1
                      do (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) stmt)))
                           (unless (or (string= trimmed "")
                                      (cl-ppcre:scan "^--" trimmed))
                             (format t "~&[DB DEBUG] Statement ~D: ~A...~%"
                                     idx
                                     (subseq trimmed 0 (min 60 (length trimmed))))
                             (handler-case
                                 (progn
                                   (postmodern:execute trimmed)
                                   (incf success-count)
                                   (format t "~&[DB DEBUG] Statement ~D succeeded~%" idx))
                               (error (e)
                                 ;; Check if it's an expected "already exists" or syntax error from incomplete statement
                                 (let ((error-msg (princ-to-string e)))
                                   (if (or (search "already exists" error-msg)
                                          (search "duplicate" error-msg))
                                       (progn
                                         (incf skip-count)
                                         (format t "~&[DB INFO] Skipping statement ~D (already exists)~%" idx))
                                       (progn
                                         (format t "~&[DB ERROR] Statement ~D failed: ~A~%" idx e)
                                         (format t "~&[DB ERROR] Full statement: ~A~%" trimmed)
                                         (error e)))))))))
                (format t "~&[DB] Schema initialized successfully (~D statements executed, ~D skipped)~%"
                        success-count skip-count)
                t)
            (error (e)
              (format t "~&[DB ERROR] Failed to initialize schema: ~A~%" e)
              nil)))))))
