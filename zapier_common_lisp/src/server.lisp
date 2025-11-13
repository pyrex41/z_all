;;;; server.lisp

(in-package #:zapier-triggers)

(defvar *server* nil "Server instance")
(defvar *app* nil "Application instance")

(defun drain-stream-safely (input)
  "CRITICAL: Drain ALL leftover bytes from stream to prevent HTTP pipelining pollution"
  (when input
    (handler-case
        (loop while (listen input)
              do (read-byte input nil nil))
      (error (e)
        (format t "~&[DEBUG] Stream drain completed or errored: ~A~%" e)))))

(defun read-body (env)
  "Read request body from environment - safely handle empty/malformed bodies.
   ALWAYS drains stream to prevent HTTP pipelining pollution."
  (let ((content-length (getf env :content-length))
        (input (getf env :raw-body))
        (method (getf env :request-method)))

    (handler-case
        (progn
          ;; Step 1: Read the expected body for POST/PUT/PATCH
          (let ((body-string
                 (if (and content-length
                          (> content-length 0)
                          input
                          (member method '(:post :put :patch)))
                     ;; Read the expected body
                     (let ((body (make-array content-length :element-type '(unsigned-byte 8))))
                       (let ((bytes-read (read-sequence body input :start 0 :end content-length)))
                         (when (> bytes-read 0)
                           (flexi-streams:octets-to-string
                            (subseq body 0 bytes-read)
                            :external-format :utf-8))))
                     nil)))

            ;; Step 2: ALWAYS drain remaining bytes from stream to prevent pollution
            ;; This is critical for preventing leftover data from affecting next request
            (when input
              (drain-stream-safely input))

            body-string))
      (error (e)
        (format t "~&[ERROR] Failed to read/drain body: ~A~%" e)
        nil))))

(defun parse-query-string (query-string)
  "Parse query string into alist"
  (when query-string
    (loop for param in (cl-ppcre:split "&" query-string)
          for parts = (cl-ppcre:split "=" param)
          when (= (length parts) 2)
          collect (cons (first parts) (second parts)))))

(defun match-route (path method)
  "Match path and method to handler. Returns (handler . path-params) or nil"
  (cond
    ;; Health check
    ((and (string= path "/health") (eq method :get))
     (cons :health-check nil))

    ;; Queue stats
    ((and (string= path "/api/queue/stats") (eq method :get))
     (cons :queue-stats nil))

    ;; API key generation
    ((and (string= path "/api/keys/generate") (eq method :post))
     (cons :generate-key nil))

    ;; API key info
    ((and (string= path "/api/keys") (eq method :get))
     (cons :get-key-info nil))

    ;; Create event
    ((and (string= path "/api/events") (eq method :post))
     (cons :create-event nil))

    ;; Get inbox
    ((and (string= path "/api/inbox") (eq method :get))
     (cons :get-inbox nil))

    ;; Acknowledge event (path parameter extraction)
    ((and (cl-ppcre:scan "^/api/ack/[^/]+$" path) (eq method :post))
     (let ((id (car (last (cl-ppcre:split "/" path)))))
       (cons :acknowledge-event (list :id id))))

    ;; Configure webhook
    ((and (string= path "/api/webhook/config") (eq method :post))
     (cons :config-webhook nil))

    ;; Not found
    (t nil)))

(defun router-handler (env)
  "Main router handler - pure function for Woo/Clack"
  (let* ((path (getf env :path-info))
         (method (getf env :request-method))
         (match (match-route path method)))
    (if match
        (let ((route-type (car match))
              (path-params (cdr match)))
          ;; Route to appropriate handler
          (case route-type
            (:health-check
             ;; CRITICAL: Always drain stream, even for GET
             (read-body env)
             (zapier-triggers.routes:health-check-handler env))

            (:queue-stats
             ;; CRITICAL: Always drain stream, even for GET
             (read-body env)
             (let ((stats (zapier-triggers:queue-stats)))
               (zapier-triggers.utils:json-success-response stats)))

            (:generate-key
             (zapier-triggers.routes:generate-key-handler
              (list :body (read-body env))))

            (:get-key-info
             ;; CRITICAL: Always drain stream, even for GET
             (read-body env)
             (zapier-triggers.routes:get-key-info-handler
              (list :env env)))

            (:create-event
             (zapier-triggers.routes:create-event-handler
              (list :env env :body (read-body env))))

            (:get-inbox
             ;; CRITICAL: Always call read-body to drain stream, even for GET
             (read-body env)
             (let* ((query-string (getf env :query-string))
                    (query-params (parse-query-string query-string)))
               (zapier-triggers.routes:get-inbox-handler
                (list :env env :query-params query-params))))

            (:acknowledge-event
             (zapier-triggers.routes:acknowledge-event-handler
              (append (list :env env) path-params)))

            (:config-webhook
             (zapier-triggers.routes:config-webhook-handler
              (list :env env :body (read-body env))))))
        ;; 404 Not Found
        (zapier-triggers.utils:json-error-response
         "Route not found"
         :status 404
         :code "not_found"))))

(defun build-app ()
  "Build application with Lack middleware stack - FUNCTIONAL COMPOSITION"
  ;; Use lack:builder with nested middleware calls wrapping pure function
  ;; This fixes the type mismatch - middleware now wraps a function, not an object
  (lack:builder
   (zapier-triggers.middleware:error-handler-middleware
    (zapier-triggers.middleware:auth-middleware
     (zapier-triggers.middleware:rate-limit-middleware
      #'router-handler)))))

(defun start-server (&key (port 5000) (worker-num 4) (debug nil) (server :woo))
  "Start HTTP server
   :server can be :woo (async, default), :hunchentoot (stable), :fcgi, or any Clack-supported server
   For :woo, uses WOO directly to ensure proper 0.0.0.0 binding for Fly.io deployment"
  (unless *server*
    ;; Initialize database connection
    (format t "~&[SERVER] Initializing database...~%")
    (zapier-triggers.db:connect-db)
    (zapier-triggers.db:init-schema)

    ;; Start background workers for async event processing
    (format t "~&[SERVER] Starting background workers...~%")
    (zapier-triggers:start-workers 2)  ; 2 background workers

    ;; Build application
    (format t "~&[SERVER] Building application...~%")
    (setf *app* (build-app))

    ;; Start server
    (format t "~&[SERVER] Starting ~A server on 0.0.0.0:~D~A~%"
            (string-upcase (symbol-name server))
            port
            (if (eq server :woo)
                (format nil " with ~D workers" worker-num)
                ""))

    (setf *server*
          (if (eq server :woo)
              ;; Use WOO directly with explicit address binding
              ;; This ensures the server binds to 0.0.0.0 for Fly.io deployment
              (woo:run *app*
                       :address "0.0.0.0"  ; Bind to all interfaces
                       :port port
                       :worker-num worker-num
                       :debug debug)
              ;; Fall back to Clack for other servers
              (clack:clackup *app*
                            :server server
                            :address "0.0.0.0"
                            :port port
                            :debug debug
                            :use-default-middlewares nil)))

    (format t "~&[SERVER] Server started successfully on 0.0.0.0:~D~%~%" port)
    (format t "~&[SERVER] Health check: http://localhost:~D/health~%" port)
    (format t "~&[SERVER] Queue stats:  http://localhost:~D/api/queue/stats~%" port)
    *server*))

(defun stop-server ()
  "Stop HTTP server"
  (when *server*
    (format t "~&[SERVER] Stopping server...~%")

    ;; Stop background workers first
    (zapier-triggers:stop-workers)

    ;; Stop server - WOO uses woo:stop, Clack uses clack:stop
    (handler-case
        (if (typep *server* 'cffi:foreign-pointer)
            ;; WOO server (returns a listener pointer)
            (woo:stop *server*)
            ;; Clack-managed server
            (clack:stop *server*))
      (error (e)
        (format t "~&[WARNING] Error stopping server: ~A~%" e)))

    (setf *server* nil)

    ;; Disconnect database
    (zapier-triggers.db:disconnect-db)

    (format t "~&[SERVER] Server stopped~%"))
  t)

(defun restart-server (&key (port 5000) (worker-num 4) (debug nil) (server :woo))
  "Restart server"
  (stop-server)
  (sleep 1)
  (start-server :port port :worker-num worker-num :debug debug :server server))

(defun main ()
  "Main entry point - keeps server running indefinitely"
  (let ((port (zapier-triggers.config:get-config :port))
        (workers (zapier-triggers.config:get-config :worker-count))
        (env (zapier-triggers.config:get-config :environment)))
    (format t "~&========================================~%")
    (format t "~&  Zapier Triggers API - Common Lisp~%")
    (format t "~&  Environment: ~A~%" env)
    (format t "~&========================================~%~%")
    (start-server :port port
                  :worker-num workers
                  :debug (string= env "development"))
    ;; WOO's run function is blocking and keeps the server alive
    ;; The loop is only needed if WOO returns (shouldn't happen)
    (format t "~&[SERVER] Server event loop started, keeping main thread alive...~%")
    (handler-case
        (loop (sleep 60))  ; Keep alive with periodic checks
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl  ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal ()
        (format t "~&[SERVER] Received interrupt, shutting down gracefully...~%")
        (stop-server)
        (uiop:quit 0)))))
