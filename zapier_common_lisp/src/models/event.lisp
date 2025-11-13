;;;; models/event.lisp

(in-package #:zapier-triggers.models)

(defun create-event (org-id event-type payload &key dedup-id)
  "Create a new event"
  (let ((event-id (zapier-triggers.utils:generate-event-id))
        (payload-json (jonathan:to-json payload :from :plist)))
    (zapier-triggers.db:with-connection
      (handler-case
          (progn
            (zapier-triggers.db::insert-event event-id org-id event-type
                                              payload-json dedup-id)
            (list :id event-id
                  :organization-id org-id
                  :type event-type
                  :payload payload
                  :status "pending"))
        (error (e)
          (format t "~&[EVENT ERROR] Failed to create event: ~A~%" e)
          nil)))))

(defun find-events (org-id &key status limit offset)
  "Find events for an organization"
  (zapier-triggers.db:with-connection
    (let ((rows (zapier-triggers.db::find-events-by-organization
                 org-id
                 :status status
                 :limit (or limit 100)
                 :offset (or offset 0))))
      (mapcar (lambda (row)
                (destructuring-bind (id org-id type payload-json status created delivered) row
                  (list :id id
                        :organization-id org-id
                        :type type
                        :payload payload-json  ; Keep as string - handler will parse if needed
                        :status status
                        :created-at created
                        :delivered-at delivered)))
              rows))))

(defun find-event-by-id (event-id)
  "Find event by ID"
  (zapier-triggers.db:with-connection
    (let ((row (zapier-triggers.db::find-event-by-id event-id)))
      (when row
        (destructuring-bind (id org-id type payload-json status created delivered) row
          (list :id id
                :organization-id org-id
                :type type
                :payload payload-json  ; Keep as string - handler will parse if needed
                :status status
                :created-at created
                :delivered-at delivered))))))

(defun update-event-status (event-id status)
  "Update event status"
  (zapier-triggers.db:with-connection
    (zapier-triggers.db::update-event-status event-id status)))

(defun event-belongs-to-org-p (event-id org-id)
  "Check if event belongs to organization"
  (let ((event (find-event-by-id event-id)))
    (and event
         (= (getf event :organization-id) org-id))))

(defun count-events (org-id &optional status)
  "Count events for an organization"
  (zapier-triggers.db:with-connection
    (zapier-triggers.db::count-events-by-organization org-id status)))
