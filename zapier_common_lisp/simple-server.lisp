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
          do (setf (gethash (string-downcase (symbol-name key)) ht) value))
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

;; API Routes
(define-easy-handler (health :uri "/health") ()
  (json-response (list :status "ok"
                       :timestamp (local-time:format-timestring
                                  nil (local-time:now)))))

(define-easy-handler (generate-key :uri "/api/keys/generate") ()
  (let* ((body (get-json-body))
         (org-name (gethash "organization_name" body))
         (tier (gethash "tier" body "free"))
         (api-key (format nil "sk_~a" (uuid:make-v4-uuid))))
    (setf (gethash api-key *api-keys*)
          (list :org-name org-name :tier tier :created (local-time:now)))
    (json-response (list :api-key api-key
                        :organization-name org-name
                        :tier tier))))

(define-easy-handler (post-event :uri "/api/events") ()
  (let* ((api-key (header-in* :x-api-key))
         (body (get-json-body))
         (org-info (gethash api-key *api-keys*)))
    (if org-info
        (let* ((event-type (gethash "type" body))
               (payload (gethash "payload" body))
               (dedup-id (gethash "dedup_id" body))
               (event-id (uuid:make-v4-uuid))
               (org-name (getf org-info :org-name)))

          ;; Check for duplicate if dedup-id provided
          (let ((existing-event-id (check-duplicate org-name dedup-id)))
            (if existing-event-id
                ;; Duplicate found - return existing event-id
                (progn
                  (format t "~&[DEDUP] Duplicate event rejected: ~a (org: ~a)~%"
                          dedup-id org-name)
                  (json-response (list :status "duplicate"
                                      :message "Event already processed"
                                      :event-id (format nil "~a" existing-event-id))
                                 200))
              ;; New event - process and cache
              (progn
                ;; Store event
                (push (list :id event-id
                           :type event-type
                           :payload payload
                           :dedup-id dedup-id
                           :timestamp (local-time:now))
                      *events*)

                ;; Mark in dedup cache
                (mark-as-seen org-name dedup-id event-id)

                (format t "~&[EVENT] Created: ~a (type: ~a, dedup: ~a)~%"
                        event-id event-type dedup-id)

                (json-response (list :status "accepted"
                                    :event-id (format nil "~a" event-id)))))))
        (json-response (list :error "Invalid API key") 401))))

(define-easy-handler (get-inbox :uri "/api/inbox") ()
  (let ((api-key (header-in* :x-api-key)))
    (if (gethash api-key *api-keys*)
        (json-response (list :events *events*
                            :count (length *events*)))
        (json-response (list :error "Invalid API key") 401))))

(define-easy-handler (cache-stats :uri "/stats/cache") ()
  "Get deduplication cache statistics"
  (json-response (get-cache-stats)))

;; Server control
(defun start-server (&key (port 5001))
  "Start the Hunchentoot server"
  (format t "~%Starting Zapier Triggers API (Hunchentoot) on port ~a...~%" port)
  (setf *server* (make-instance 'easy-acceptor :port port))
  (start *server*)
  (format t "Server running at http://localhost:~a~%" port)
  (format t "Try: curl http://localhost:~a/health~%" port))

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
