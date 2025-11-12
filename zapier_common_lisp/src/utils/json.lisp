;;;; utils/json.lisp

(in-package #:zapier-triggers.utils)

(defun normalize-plist-keys (plist)
  "Convert plist keywords to lowercase strings for JSON output"
  (loop for (key value) on plist by #'cddr
        collect (if (keywordp key)
                    (intern (string-downcase (symbol-name key)) :keyword)
                    key)
        collect value))

(defun parse-json-body (body-string)
  "Parse JSON string into plist/alist"
  (handler-case
      (jonathan:parse body-string :as :plist)
    (error (e)
      (format t "~&[JSON ERROR] Failed to parse: ~A~%" e)
      nil)))

(defun make-json-response (data &key (status 200) headers)
  "Create JSON response with proper headers"
  (let ((normalized-data (normalize-plist-keys data)))
    (list status
          (append '(:content-type "application/json"
                    :connection "close")  ; Disable keep-alive to prevent buffer pollution
                  headers)
          (list (jonathan:to-json normalized-data :from :plist)))))

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
