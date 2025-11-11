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

(defpackage :zapier-simple
  (:use :cl :hunchentoot)
  (:export #:start-server #:stop-server))

(in-package :zapier-simple)

(defvar *server* nil "The Hunchentoot server instance")
(defvar *api-keys* (make-hash-table :test 'equal) "Simple in-memory API key storage")
(defvar *events* nil "Simple in-memory event storage")
(defvar *db-connection* nil "Database connection")
(defvar *dedup-cache* (make-hash-table :test 'equal) "Deduplication cache")
(defvar *dedup-max-size* 10000 "Maximum dedup cache entries before eviction")
(defvar *dedup-lock* (bt:make-lock "dedup-cache-lock") "Thread-safe lock for dedup cache")

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

;; Database setup
(defun connect-db ()
  "Connect to PostgreSQL database"
  (setf pomo:*database*
        (pomo:connect "zapier_triggers" "postgres" "" "localhost")))

;; Database queries
(defun db-create-organization (org-name api-key tier)
  "Create organization in database and return org-id"
  (pomo:with-connection '("zapier_triggers" "postgres" "" "localhost")
    (pomo:query
     "INSERT INTO organizations (name, api_key, tier, created_at)
      VALUES ($1, $2, $3, NOW())
      RETURNING id"
     org-name api-key tier
     :single)))

(defun db-validate-api-key (api-key)
  "Validate API key against database. Returns (id name tier) or NIL"
  (pomo:with-connection '("zapier_triggers" "postgres" "" "localhost")
    (pomo:query
     "SELECT id, name, tier FROM organizations WHERE api_key = $1"
     api-key
     :row)))

(defun db-insert-event (event-id org-id event-type payload-json dedup-id)
  "Insert event into database. Returns event-id or signals duplicate error"
  (handler-case
      (pomo:with-connection '("zapier_triggers" "postgres" "" "localhost")
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
  (pomo:with-connection '("zapier_triggers" "postgres" "" "localhost")
    (pomo:query
     "SELECT id, event_type, payload, dedup_id, status, created_at
      FROM events
      WHERE organization_id = $1 AND status = $2
      ORDER BY created_at DESC
      LIMIT $3"
     org-id status limit
     :rows)))

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
(defun start-server (&key (port 5001) (wait t))
  "Start the Hunchentoot server"
  (format t "~%Starting Zapier Triggers API (Hunchentoot) on port ~a...~%" port)
  (setf *server* (make-instance 'easy-acceptor :port port))
  (start *server*)
  (format t "Server running at http://localhost:~a~%" port)
  (format t "Try: curl http://localhost:~a/health~%" port)
  (when wait
    (format t "~%Press Ctrl+C to stop the server.~%")
    (handler-case
        (loop (sleep 60))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       ()
       (format t "~%Shutting down...~%")
       (stop-server)))))

(defun stop-server ()
  "Stop the Hunchentoot server"
  (when *server*
    (stop *server*)
    (setf *server* nil)
    (format t "~%Server stopped.~%")))

;; Auto-start when loaded
(format t "~%Loading Zapier Triggers API...~%")
(format t "To start: (zapier-simple:start-server :port 5001)~%")
(format t "To stop:  (zapier-simple:stop-server)~%~%")
