;; Register local project directory
(push (truename ".") asdf:*central-registry*)

;; Load the system
(ql:quickload :zapier-triggers :silent nil)

;; Switch to package
(in-package :zapier-triggers)

;; Parse command line arguments for server type
(defparameter *server-type*
  (let ((arg (second sb-ext:*posix-argv*)))
    (cond
      ((string= arg "woo") :woo)
      ((string= arg "hunchentoot") :hunchentoot)
      ((string= arg "fcgi") :fcgi)
      (t :woo)))) ; default to Woo

;; Start the server
(format t "~%Starting Zapier Triggers API on port 5001...~%")
(format t "Server type: ~A~%" (string-upcase (symbol-name *server-type*)))
(zapier-triggers:start-server :port 5001 :worker-num 4 :debug t :server *server-type*)

;; Keep running
(loop (sleep 1))
