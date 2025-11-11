;; Register local project directory
(push (truename ".") asdf:*central-registry*)

;; Load the system
(ql:quickload :zapier-triggers :silent nil)

;; Switch to package
(in-package :zapier-triggers)

;; Start the server
(format t "~%Starting Zapier Triggers API on port 5001...~%")
(zapier-triggers:start-server :port 5001 :worker-num 2 :debug t)

;; Keep running
(loop (sleep 1))
