;;;; utils/json.lisp

(in-package #:zapier-triggers.utils)

(defun parse-json-body (body-string)
  "Parse JSON string into plist/alist"
  (handler-case
      (jonathan:parse body-string :as :plist)
    (error (e)
      (format t "~&[JSON ERROR] Failed to parse: ~A~%" e)
      nil)))

(defun make-json-response (data &key (status 200) headers)
  "Create JSON response with proper headers"
  (list status
        (append '(:content-type "application/json")
                headers)
        (list (jonathan:to-json data :from :plist))))

(defun json-error-response (message &key (status 400) (code "error"))
  "Create standardized JSON error response"
  (make-json-response
   (list :error t
         :code code
         :message message)
   :status status))

(defun json-success-response (data &key (status 200))
  "Create standardized JSON success response"
  (make-json-response data :status status))
