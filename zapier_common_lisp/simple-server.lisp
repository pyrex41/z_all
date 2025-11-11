;;;; simple-server.lisp - Simplified server using Hunchentoot
;;;; This version uses only Debian-available packages

(require :asdf)

;; Add system directories
(push #p"/usr/share/common-lisp/systems/" asdf:*central-registry*)
(push #p"/usr/share/common-lisp/source/" asdf:*central-registry*)

;; Load dependencies
(asdf:load-system :hunchentoot)
(asdf:load-system :yason)
(asdf:load-system :postmodern)
(asdf:load-system :bordeaux-threads)
(asdf:load-system :local-time)
(asdf:load-system :uuid)
(asdf:load-system :ironclad)
(asdf:load-system :cl-ppcre)
(asdf:load-system :drakma)

(defpackage :zapier-simple
  (:use :cl :hunchentoot)
  (:export #:start-server #:stop-server))

(in-package :zapier-simple)

;; Configuration
(defvar *config* (make-hash-table :test 'equal) "Application configuration")

(defun load-config ()
  "Load configuration from environment or defaults"
  (setf (gethash "db-name" *config*) (or (uiop:getenv "DB_NAME") "zapier_triggers"))
  (setf (gethash "db-user" *config*) (or (uiop:getenv "DB_USER") "postgres"))
  (setf (gethash "db-pass" *config*) (or (uiop:getenv "DB_PASS") ""))
  (setf (gethash "db-host" *config*) (or (uiop:getenv "DB_HOST") "localhost"))
  (setf (gethash "db-port" *config*) (parse-integer (or (uiop:getenv "DB_PORT") "5432")))
  (setf (gethash "port" *config*) (parse-integer (or (uiop:getenv "PORT") "5001")))
  (setf (gethash "dedup-max-size" *config*) (parse-integer (or (uiop:getenv "DEDUP_MAX_SIZE") "10000")))
  (setf (gethash "rate-limit-rpm" *config*) (parse-integer (or (uiop:getenv "RATE_LIMIT_RPM") "1000"))))

(defun get-config (key)
  "Get configuration value"
  (gethash key *config*))

;; State
(defvar *server* nil "The Hunchentoot server instance")
(defvar *api-keys* (make-hash-table :test 'equal) "Simple in-memory API key storage")
(defvar *events* nil "Simple in-memory event storage")
(defvar *db-connection* nil "Database connection")
(defvar *dedup-cache* (make-hash-table :test 'equal) "Deduplication cache")
(defvar *dedup-max-size* 10000 "Maximum dedup cache entries before eviction")
(defvar *dedup-lock* (bt:make-lock "dedup-cache-lock") "Thread-safe lock for dedup cache")

;; Connection pool
(defvar *db-pool* (make-array 10 :initial-element nil) "Database connection pool")
(defvar *db-pool-available* nil "List of available connection indices")
(defvar *db-pool-lock* (bt:make-lock "db-pool-lock") "Lock for connection pool")
(defvar *db-pool-size* 10 "Maximum database connections in pool")

;; Rate limiting
(defvar *rate-limiter* (make-hash-table :test 'equal) "Rate limiter: org-id -> (count . timestamp)")
(defvar *rate-limit-lock* (bt:make-lock "rate-limit-lock") "Lock for rate limiter")

;; Connection Pool Management
(defun init-connection-pool ()
  "Initialize database connection pool"
  (bt:with-lock-held (*db-pool-lock*)
    (format t "~&[POOL] Initializing connection pool (size: ~a)...~%" *db-pool-size*)
    (setf *db-pool-available* (loop for i from 0 below *db-pool-size* collect i))
    (format t "~&[POOL] Connection pool initialized~%")))

(defun get-db-connection ()
  "Get a database connection from the pool (creates on demand)"
  (let ((idx nil))
    (bt:with-lock-held (*db-pool-lock*)
      (setf idx (pop *db-pool-available*)))
    (when (null idx)
      (error "Connection pool exhausted"))
    (unless (aref *db-pool* idx)
      ;; Create connection on demand
      (setf (aref *db-pool* idx)
            (pomo:connect (get-config "db-name")
                         (get-config "db-user")
                         (get-config "db-pass")
                         (get-config "db-host")
                         :port (get-config "db-port"))))
    (values (aref *db-pool* idx) idx)))

(defun release-db-connection (idx)
  "Return a connection to the pool"
  (bt:with-lock-held (*db-pool-lock*)
    (push idx *db-pool-available*)))

(defmacro with-pooled-connection ((conn) &body body)
  "Execute body with a pooled database connection"
  (let ((idx-var (gensym "IDX")))
    `(let ((,idx-var nil))
       (unwind-protect
           (multiple-value-bind (,conn ,idx-var) (get-db-connection)
             (let ((pomo:*database* ,conn))
               ,@body))
         (when ,idx-var
           (release-db-connection ,idx-var))))))

(defun close-connection-pool ()
  "Close all connections in the pool"
  (bt:with-lock-held (*db-pool-lock*)
    (format t "~&[POOL] Closing connection pool...~%")
    (loop for i from 0 below *db-pool-size*
          when (aref *db-pool* i)
          do (pomo:disconnect (aref *db-pool* i))
             (setf (aref *db-pool* i) nil))
    (format t "~&[POOL] Connection pool closed~%")))

;; Rate Limiting
(defun check-rate-limit (org-id)
  "Check if organization has exceeded rate limit. Returns T if allowed, NIL if blocked"
  (let ((now (get-universal-time))
        (limit-rpm (get-config "rate-limit-rpm"))
        (window 60)) ; 60 second window
    (bt:with-lock-held (*rate-limit-lock*)
      (let ((entry (gethash org-id *rate-limiter*)))
        (if (null entry)
            ;; First request
            (progn
              (setf (gethash org-id *rate-limiter*) (cons 1 now))
              t)
            (destructuring-bind (count . timestamp) entry
              (if (> (- now timestamp) window)
                  ;; Reset window
                  (progn
                    (setf (gethash org-id *rate-limiter*) (cons 1 now))
                    t)
                  ;; Within window
                  (if (>= count limit-rpm)
                      ;; Rate limited
                      (progn
                        (format t "~&[RATE] Organization ~a exceeded rate limit (~a/min)~%"
                                org-id limit-rpm)
                        nil)
                      ;; Increment count
                      (progn
                        (setf (gethash org-id *rate-limiter*) (cons (1+ count) timestamp))
                        t)))))))))

;; Utilities
(defun plist-to-hash (plist)
  "Convert a property list to a hash table for yason"
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name key)) ht)
                   (if (and (listp value) (keywordp (first value)))
                       (plist-to-hash value)
                       value)))
    ht))

(defun make-event-hash (id type payload dedup-id status created-at)
  "Create a hash table for an event (Yason-compatible)"
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "id" ht) id)
    (setf (gethash "type" ht) type)
    (setf (gethash "payload" ht) payload)
    (setf (gethash "dedup_id" ht) dedup-id)
    (setf (gethash "status" ht) status)
    (setf (gethash "created_at" ht) created-at)
    ht))

(defun json-response (data &optional (status 200))
  "Create a JSON HTTP response"
  (setf (content-type*) "application/json")
  (setf (return-code*) status)
  (with-output-to-string (s)
    (yason:encode (plist-to-hash data) s)))

(defun get-json-body ()
  "Parse JSON from request body"
  (handler-case
      (let ((body-string (raw-post-data :force-text t)))
        (when (and body-string (> (length body-string) 0))
          (yason:parse body-string)))
    (error (e)
      (format t "Error parsing JSON: ~a~%" e)
      nil)))

;; Deduplication utilities
(defun make-dedup-key (org-id dedup-id)
  "Create cache key for deduplication"
  (format nil "~a:~a" org-id dedup-id))

(defun check-duplicate (org-id dedup-id)
  "Check if event with dedup-id already exists (thread-safe). Returns event-id if duplicate, NIL otherwise."
  (when dedup-id
    (bt:with-lock-held (*dedup-lock*)
      (gethash (make-dedup-key org-id dedup-id) *dedup-cache*))))

(defun mark-as-seen (org-id dedup-id event-id)
  "Add event to dedup cache (thread-safe). Implements simple cache eviction when full."
  (when dedup-id
    (bt:with-lock-held (*dedup-lock*)
      ;; Simple cache eviction: clear all when limit reached
      (when (>= (hash-table-count *dedup-cache*) *dedup-max-size*)
        (format t "~&[CACHE] Evicting dedup cache (size: ~a)~%"
                (hash-table-count *dedup-cache*))
        (clrhash *dedup-cache*))
      ;; Store new entry
      (setf (gethash (make-dedup-key org-id dedup-id) *dedup-cache*) event-id))))

(defun get-cache-stats ()
  "Get deduplication cache statistics"
  (bt:with-lock-held (*dedup-lock*)
    (list :size (hash-table-count *dedup-cache*)
          :max-size *dedup-max-size*
          :utilization (format nil "~,1f%"
                              (* 100 (/ (hash-table-count *dedup-cache*)
                                       *dedup-max-size*))))))

;; Database queries (using connection pool)
(defun db-create-organization (org-name api-key tier)
  "Create organization in database and return org-id"
  (with-pooled-connection (conn)
    (pomo:query
     "INSERT INTO organizations (name, api_key, tier, created_at)
      VALUES ($1, $2, $3, NOW())
      RETURNING id"
     org-name api-key tier
     :single)))

(defun db-validate-api-key (api-key)
  "Validate API key against database. Returns (id name tier) or NIL"
  (with-pooled-connection (conn)
    (pomo:query
     "SELECT id, name, tier FROM organizations WHERE api_key = $1"
     api-key
     :row)))

(defun db-insert-event (event-id org-id event-type payload-json dedup-id)
  "Insert event into database. Returns event-id or signals duplicate error"
  (handler-case
      (with-pooled-connection (conn)
        (pomo:query
         "INSERT INTO events (id, organization_id, event_type, payload, dedup_id, status, created_at)
          VALUES ($1, $2, $3, $4, $5, 'pending', NOW())
          RETURNING id"
         event-id org-id event-type payload-json dedup-id
         :single))
    (cl-postgres-error:unique-violation (e)
      (format t "~&[DB] Duplicate event in database: ~a~%" dedup-id)
      nil)))

(defun db-get-events (org-id &key (limit 100) (status "pending"))
  "Get events for organization from database"
  (with-pooled-connection (conn)
    (pomo:query
     "SELECT id, event_type, payload, dedup_id, status, created_at
      FROM events
      WHERE organization_id = $1 AND status = $2
      ORDER BY created_at DESC
      LIMIT $3"
     org-id status limit
     :rows)))

(defun db-get-webhooks (org-id)
  "Get active webhooks for organization"
  (with-pooled-connection (conn)
    (pomo:query
     "SELECT id, url
      FROM webhooks
      WHERE organization_id = $1"
     org-id
     :rows)))

(defun db-update-event-status (event-id new-status)
  "Update event status"
  (with-pooled-connection (conn)
    (pomo:query
     "UPDATE events SET status = $2, updated_at = NOW()
      WHERE id = $1"
     event-id new-status
     :none)))

;; Webhook Delivery
(defun deliver-webhook (webhook-url event-data)
  "Deliver event to webhook URL via HTTP POST"
  (handler-case
      (let ((json-payload (with-output-to-string (s)
                           (yason:encode event-data s))))
        (multiple-value-bind (body status-code)
            (drakma:http-request webhook-url
                                :method :post
                                :content-type "application/json"
                                :content json-payload
                                :connection-timeout 5
                                :read-timeout 10)
          (if (and (>= status-code 200) (< status-code 300))
              (progn
                (format t "~&[WEBHOOK] Delivered to ~a (status: ~a)~%" webhook-url status-code)
                t)
              (progn
                (format t "~&[WEBHOOK] Failed to deliver to ~a (status: ~a)~%" webhook-url status-code)
                nil))))
    (error (e)
      (format t "~&[WEBHOOK] Error delivering to ~a: ~a~%" webhook-url e)
      nil)))

(defun process-webhooks (org-id event-id event-type payload)
  "Process webhooks for an event (runs in background thread)"
  (bt:make-thread
   (lambda ()
     (handler-case
         (let ((webhooks (db-get-webhooks org-id)))
           (loop for webhook in webhooks
                 do (destructuring-bind (webhook-id url) webhook
                      (let ((event-data (make-hash-table :test 'equal)))
                        (setf (gethash "event_id" event-data) event-id)
                        (setf (gethash "event_type" event-data) event-type)
                        (setf (gethash "payload" event-data) payload)
                        (setf (gethash "timestamp" event-data)
                              (local-time:format-timestring nil (local-time:now)))
                        (let ((success (deliver-webhook url event-data)))
                          (when success
                            (db-update-event-status event-id "delivered")))))))
       (error (e)
         (format t "~&[WEBHOOK] Error processing webhooks: ~a~%" e))))
   :name (format nil "webhook-~a" event-id)))

;; API Routes
(define-easy-handler (health :uri "/health") ()
  (json-response (list :status "ok"
                       :timestamp (local-time:format-timestring
                                  nil (local-time:now)))))

(define-easy-handler (generate-key :uri "/api/keys/generate") ()
  (handler-case
      (let* ((body (get-json-body))
             (org-name (gethash "organization_name" body))
             (tier (or (gethash "tier" body) "free"))
             (api-key (format nil "sk_~a" (uuid:make-v4-uuid))))

        ;; Store in database (with in-memory fallback for compatibility)
        (let ((org-id (db-create-organization org-name api-key tier)))
          (when org-id
            ;; Also cache in memory for backward compatibility
            (setf (gethash api-key *api-keys*)
                  (list :org-id org-id :org-name org-name :tier tier :created (local-time:now))))

          (format t "~&[AUTH] Created API key for org: ~a (id: ~a)~%" org-name org-id)
          (json-response (list :api-key api-key
                              :organization-name org-name
                              :tier tier))))
    (error (e)
      (format t "~&[ERROR] Failed to generate API key: ~a~%" e)
      (json-response (list :error "Failed to generate API key"
                          :message (format nil "~a" e))
                     500))))

(define-easy-handler (post-event :uri "/api/events") ()
  (handler-case
      (let* ((api-key (header-in* :x-api-key))
             (body (get-json-body)))

        (unless api-key
          (return-from post-event
            (json-response (list :error "Missing API key") 401)))

        ;; Validate API key against database
        (let ((org-info (db-validate-api-key api-key)))
          (unless org-info
            (return-from post-event
              (json-response (list :error "Invalid API key") 401)))

          (destructuring-bind (org-id org-name tier) org-info
            ;; Check rate limit
            (unless (check-rate-limit org-id)
              (return-from post-event
                (json-response (list :error "Rate limit exceeded"
                                    :message (format nil "Maximum ~a requests per minute"
                                                   (get-config "rate-limit-rpm")))
                               429)))

            (let* ((event-type (gethash "type" body))
                   (payload (gethash "payload" body))
                   (dedup-id (gethash "dedup_id" body))
                   (event-id (format nil "~a" (uuid:make-v4-uuid))))

              ;; Check cache for duplicate
              (let ((existing-event-id (check-duplicate org-id dedup-id)))
                (if existing-event-id
                    ;; Duplicate found in cache
                    (progn
                      (format t "~&[DEDUP] Duplicate event (cache): ~a (org: ~a)~%"
                              dedup-id org-name)
                      (json-response (list :status "duplicate"
                                          :message "Event already processed"
                                          :event-id existing-event-id)
                                     200))

                    ;; Not in cache - try database insert
                    (let* ((payload-json (with-output-to-string (s)
                                          (yason:encode payload s)))
                           (db-event-id (db-insert-event event-id org-id event-type
                                                        payload-json dedup-id)))
                      (if db-event-id
                          ;; Successfully inserted
                          (progn
                            ;; Mark in dedup cache
                            (mark-as-seen org-id dedup-id event-id)

                            ;; Also keep in memory for compatibility
                            (push (list :id event-id
                                       :org-id org-id
                                       :type event-type
                                       :payload payload
                                       :dedup-id dedup-id
                                       :timestamp (local-time:now))
                                  *events*)

                            (format t "~&[EVENT] Created: ~a (type: ~a, org: ~a, dedup: ~a)~%"
                                    event-id event-type org-name dedup-id)

                            ;; Process webhooks in background
                            (process-webhooks org-id event-id event-type payload)

                            (json-response (list :status "accepted"
                                                :event-id event-id)))

                          ;; Database rejected (duplicate constraint)
                          (progn
                            (format t "~&[DEDUP] Duplicate event (database): ~a (org: ~a)~%"
                                    dedup-id org-name)
                            (json-response (list :status "duplicate"
                                                :message "Event already processed"
                                                :event-id event-id)
                                           200))))))))))
    (error (e)
      (format t "~&[ERROR] Failed to create event: ~a~%" e)
      (json-response (list :error "Failed to create event"
                          :message (format nil "~a" e))
                     500))))

(define-easy-handler (get-inbox :uri "/api/inbox") ()
  (handler-case
      (let ((api-key (header-in* :x-api-key)))
        (unless api-key
          (return-from get-inbox
            (json-response (list :error "Missing API key") 401)))

        ;; Validate API key against database
        (let ((org-info (db-validate-api-key api-key)))
          (unless org-info
            (return-from get-inbox
              (json-response (list :error "Invalid API key") 401)))

          (destructuring-bind (org-id org-name tier) org-info
            ;; Get query parameters
            (let* ((limit-str (parameter "limit"))
                   (status (or (parameter "status") "pending"))
                   (limit (if limit-str
                             (parse-integer limit-str :junk-allowed t)
                             100)))

              ;; Query events from database
              (let ((events (db-get-events org-id :limit limit :status status)))
                (format t "~&[INBOX] Retrieved ~a events for org: ~a~%"
                        (length events) org-name)

                ;; Create response hash table directly
                (let ((response (make-hash-table :test 'equal))
                      (event-list (mapcar
                                   (lambda (row)
                                     (destructuring-bind (id type payload-json dedup status created) row
                                       (make-event-hash id type
                                                       (yason:parse payload-json)
                                                       dedup status
                                                       (format nil "~a" created))))
                                   events)))
                  (setf (gethash "events" response) event-list)
                  (setf (gethash "count" response) (length events))
                  (setf (content-type*) "application/json")
                  (setf (return-code*) 200)
                  (with-output-to-string (s)
                    (yason:encode response s))))))))
    (error (e)
      (format t "~&[ERROR] Failed to retrieve inbox: ~a~%" e)
      (json-response (list :error "Failed to retrieve inbox"
                          :message (format nil "~a" e))
                     500))))

(define-easy-handler (cache-stats :uri "/stats/cache") ()
  "Get deduplication cache statistics"
  (json-response (get-cache-stats)))

;; Server control
(defun start-server (&key (port nil) (wait t))
  "Start the Hunchentoot server with configuration and connection pool"
  (format t "~%===== Zapier Triggers API (Common Lisp) =====~%")

  ;; Load configuration
  (format t "~&[CONFIG] Loading configuration...~%")
  (load-config)
  (let ((actual-port (or port (get-config "port"))))
    (format t "~&[CONFIG] Port: ~a~%" actual-port)
    (format t "~&[CONFIG] Database: ~a@~a~%"
            (get-config "db-name") (get-config "db-host"))
    (format t "~&[CONFIG] Rate limit: ~a req/min~%"
            (get-config "rate-limit-rpm"))

    ;; Initialize connection pool
    (init-connection-pool)

    ;; Start HTTP server
    (format t "~&[SERVER] Starting Hunchentoot on port ~a...~%" actual-port)
    (setf *server* (make-instance 'easy-acceptor :port actual-port))
    (start *server*)
    (format t "~&[SERVER] Server running at http://localhost:~a~%" actual-port)
    (format t "~&[SERVER] Health check: curl http://localhost:~a/health~%" actual-port)
    (format t "~%Server ready!~%")

    (when wait
      (format t "~%Press Ctrl+C to stop the server.~%~%")
      (handler-case
          (loop (sleep 60))
        (#+sbcl sb-sys:interactive-interrupt
         #+ccl ccl:interrupt-signal-condition
         #+clisp system::simple-interrupt-condition
         #+ecl ext:interactive-interrupt
         #+allegro excl:interrupt-signal
         ()
         (format t "~%Shutting down...~%")
         (stop-server))))))

(defun stop-server ()
  "Stop the Hunchentoot server and close connection pool"
  (when *server*
    (format t "~&[SERVER] Stopping HTTP server...~%")
    (stop *server*)
    (setf *server* nil))
  (close-connection-pool)
  (format t "~%Server stopped.~%"))

;; Auto-start when loaded
(format t "~%Loading Zapier Triggers API...~%")
(format t "To start: (zapier-simple:start-server :port 5001)~%")
(format t "To stop:  (zapier-simple:stop-server)~%~%")
