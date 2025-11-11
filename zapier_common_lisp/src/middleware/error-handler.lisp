;;;; middleware/error-handler.lisp

(in-package #:zapier-triggers.middleware)

(defun error-handler-middleware (app)
  "Middleware to handle errors gracefully"
  (lambda (env)
    (handler-case
        (funcall app env)
      (error (e)
        (let ((error-msg (format nil "~A" e))
              (backtrace (trivial-backtrace:print-backtrace e :output nil)))
          (format t "~&[ERROR] ~A~%~A~%" error-msg backtrace)
          (zapier-triggers.utils:json-error-response
           "Internal server error"
           :status 500
           :code "internal_error"))))))
