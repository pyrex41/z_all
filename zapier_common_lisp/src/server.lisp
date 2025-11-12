;;;; server.lisp

(in-package #:zapier-triggers)

(defvar *server* nil "Server instance")
(defvar *app* nil "Application instance")

(defun read-body (env)
  "Read request body from environment"
  (let ((content-length (getf env :content-length))
        (input (getf env :raw-body)))
    (when (and content-length (> content-length 0))
      (let ((body (make-string content-length)))
        (read-sequence body input)
        body))))

(defun parse-query-string (query-string)
  "Parse query string into alist"
  (when query-string
    (loop for param in (cl-ppcre:split "&" query-string)
          for parts = (cl-ppcre:split "=" param)
          when (= (length parts) 2)
          collect (cons (first parts) (second parts)))))

(defun setup-routes ()
  "Setup application routes with Ningle"
  (let ((app (make-instance 'ningle:<app>)))

    ;; Health check
    (setf (ningle:route app "/health" :method :GET)
          (lambda (params)
            (zapier-triggers.routes:health-check-handler params)))

    ;; Queue stats (for monitoring)
    (setf (ningle:route app "/api/queue/stats" :method :GET)
          (lambda (params)
            (declare (ignore params))
            (let ((stats (zapier-triggers:queue-stats)))
              (zapier-triggers.utils:json-success-response stats))))

    ;; API key generation
    (setf (ningle:route app "/api/keys/generate" :method :POST)
          (lambda (params)
            (let ((env (getf params :_env)))
              (zapier-triggers.routes:generate-key-handler
               (list :body (read-body env))))))

    ;; API key info
    (setf (ningle:route app "/api/keys" :method :GET)
          (lambda (params)
            (let ((env (getf params :_env)))
              (zapier-triggers.routes:get-key-info-handler
               (list :env env)))))

    ;; Create event
    (setf (ningle:route app "/api/events" :method :POST)
          (lambda (params)
            (let ((env (getf params :_env)))
              (zapier-triggers.routes:create-event-handler
               (list :env env :body (read-body env))))))

    ;; Get inbox
    (setf (ningle:route app "/api/inbox" :method :GET)
          (lambda (params)
            (let* ((env (getf params :_env))
                   (query-string (getf env :query-string))
                   (query-params (parse-query-string query-string)))
              (zapier-triggers.routes:get-inbox-handler
               (list :env env :query-params query-params)))))

    ;; Acknowledge event
    (setf (ningle:route app "/api/ack/:id" :method :POST)
          (lambda (params)
            (let ((env (getf params :_env))
                  (event-id (cdr (assoc :id params))))
              (zapier-triggers.routes:acknowledge-event-handler
               (list :env env :id event-id)))))

    ;; Configure webhook
    (setf (ningle:route app "/api/webhook/config" :method :POST)
          (lambda (params)
            (let ((env (getf params :_env)))
              (zapier-triggers.routes:config-webhook-handler
               (list :env env :body (read-body env))))))

    app))

(defun build-app ()
  "Build application with middleware stack"
  (lack:builder
   ;; Error handler (outermost)
   :accesslog
   (zapier-triggers.middleware:error-handler-middleware
    ;; Authentication
    (zapier-triggers.middleware:auth-middleware
     ;; Rate limiting
     (zapier-triggers.middleware:rate-limit-middleware
      ;; Routes (innermost)
      (setup-routes))))))

(defun start-server (&key (port 5000) (worker-num 4) (debug nil) (server :woo))
  "Start HTTP server using Clack interface
   :server can be :woo (default), :hunchentoot, :fcgi, or any Clack-supported server"
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

    ;; Start server using Clack
    (format t "~&[SERVER] Starting ~A server on port ~D~A~%"
            (string-upcase (symbol-name server))
            port
            (if (eq server :woo)
                (format nil " with ~D workers" worker-num)
                ""))
    (setf *server*
          (clack:clackup *app*
                        :server server
                        :port port
                        :worker-num (if (eq server :woo) worker-num nil)
                        :debug debug
                        :use-default-middlewares nil))

    (format t "~&[SERVER] Server started successfully~%")
    (format t "~&[SERVER] Visit http://localhost:~D/health~%" port)
    (format t "~&[SERVER] Queue stats: http://localhost:~D/api/queue/stats~%" port)
    *server*))

(defun stop-server ()
  "Stop HTTP server (Clack-managed)"
  (when *server*
    (format t "~&[SERVER] Stopping server...~%")

    ;; Stop background workers first
    (zapier-triggers:stop-workers)

    ;; Stop server using Clack
    (clack:stop *server*)
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
  "Main entry point"
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
    ;; Keep main thread alive
    (loop (sleep 1))))
