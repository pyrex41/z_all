;;;; utils/crypto.lisp

(in-package #:zapier-triggers.utils)

(defun generate-uuid ()
  "Generate UUID v4"
  (format nil "~(~A~)" (uuid:make-v4-uuid)))

(defun generate-api-key ()
  "Generate secure API key (UUID v4)"
  (generate-uuid))

(defun generate-event-id ()
  "Generate unique event ID"
  (generate-uuid))
