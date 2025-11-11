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
         (body (get-json-body)))
    (if (gethash api-key *api-keys*)
        (progn
          (push (list :id (uuid:make-v4-uuid)
                     :type (gethash "type" body)
                     :payload (gethash "payload" body)
                     :timestamp (local-time:now))
                *events*)
          (json-response (list :status "accepted"
                              :event-id (first (first *events*)))))
        (json-response (list :error "Invalid API key") 401))))

(define-easy-handler (get-inbox :uri "/api/inbox") ()
  (let ((api-key (header-in* :x-api-key)))
    (if (gethash api-key *api-keys*)
        (json-response (list :events *events*
                            :count (length *events*)))
        (json-response (list :error "Invalid API key") 401))))

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
