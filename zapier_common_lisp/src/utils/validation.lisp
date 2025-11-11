;;;; utils/validation.lisp

(in-package #:zapier-triggers.utils)

(defun validate-event (event-data)
  "Validate event payload structure"
  (and (listp event-data)
       (getf event-data :type)
       (getf event-data :payload)
       (stringp (getf event-data :type))
       (listp (getf event-data :payload))))

(defun validate-url (url)
  "Validate URL format"
  (and (stringp url)
       (> (length url) 0)
       (or (cl-ppcre:scan "^https?://" url)
           nil)))

(defun validate-api-key-format (api-key)
  "Validate API key format (UUID v4)"
  (and (stringp api-key)
       (= (length api-key) 36)
       (cl-ppcre:scan "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
                      api-key)))

(defun validate-tier (tier)
  "Validate organization tier"
  (member tier '("free" "starter" "professional" "enterprise") :test #'string=))

(defun validate-event-type (event-type)
  "Validate event type format"
  (and (stringp event-type)
       (> (length event-type) 0)
       (<= (length event-type) 100)))

(defun validate-payload-size (payload-string)
  "Validate payload size (max 256KB)"
  (< (length payload-string) (* 256 1024)))
