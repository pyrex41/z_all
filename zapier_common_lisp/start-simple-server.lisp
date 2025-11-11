;;;; Start script for simple-server.lisp
;;;; Loads Quicklisp, then loads dependencies, then starts server

;; Load Quicklisp
(load "~/quicklisp/setup.lisp")

;; Load dependencies via Quicklisp
(ql:quickload :hunchentoot :silent t)
(ql:quickload :yason :silent t)
(ql:quickload :postmodern :silent t)
(ql:quickload :bordeaux-threads :silent t)
(ql:quickload :local-time :silent t)
(ql:quickload :uuid :silent t)
(ql:quickload :ironclad :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :drakma :silent t)

;; Load the simple server
(load "/Users/reuben/gauntlet/zapier/zapier_common_lisp/simple-server.lisp")

;; Start the server
(format t "~%~%========================================~%")
(format t "Starting Zapier Triggers API (Simple)~%")
(format t "========================================~%~%")

(zapier-simple:start-server)

;; Keep the main thread alive
(sb-thread:join-thread
  (find-if (lambda (th) (search "hunchentoot" (sb-thread:thread-name th)))
           (sb-thread:list-all-threads)))
