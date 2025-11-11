;;;; routes/events.lisp

(in-package #:zapier-triggers.routes)

(defun create-event-handler (params)
  "Create event endpoint handler"
  (let* ((env (getf params :env))
         (body (getf params :body))
         (org (getf env :organization))
         (org-id (getf org :id)))

    (unless org-id
      (return-from create-event-handler
        (zapier-triggers.utils:json-error-response
         "Organization not found"
         :status 401
         :code "unauthorized")))

    ;; Validate payload size
    (unless (zapier-triggers.utils:validate-payload-size body)
      (return-from create-event-handler
        (zapier-triggers.utils:json-error-response
         "Payload too large (max 256KB)"
         :status 413
         :code "payload_too_large")))

    ;; Parse and validate event data
    (let ((data (zapier-triggers.utils:parse-json-body body)))
      (unless data
        (return-from create-event-handler
          (zapier-triggers.utils:json-error-response
           "Invalid JSON"
           :status 400
           :code "invalid_json")))

      (let ((event-type (or (getf data :|type|) (getf data :type)))
            (payload (or (getf data :|payload|) (getf data :payload)))
            (dedup-id (or (getf data :|dedup_id|) (getf data :dedup-id))))

        ;; Validate event type
        (unless (and event-type (zapier-triggers.utils:validate-event-type event-type))
          (return-from create-event-handler
            (zapier-triggers.utils:json-error-response
             "Invalid or missing event type"
             :status 400
             :code "invalid_event_type")))

        ;; Validate payload
        (unless payload
          (return-from create-event-handler
            (zapier-triggers.utils:json-error-response
             "Missing payload"
             :status 400
             :code "missing_payload")))

        ;; Enqueue event for async processing (instant response)
        (let ((event-id (zapier-triggers:enqueue-event
                         org-id event-type payload dedup-id)))
          (zapier-triggers.utils:json-success-response
           (list :|id| event-id
                 :|type| event-type
                 :|status| "accepted"
                 :|message| "Event queued for processing"
                 :|created_at| (local-time:format-timestring
                               nil (local-time:now)))
           :status 202))))))  ; 202 Accepted for async processing

(defun acknowledge-event-handler (params)
  "Acknowledge event endpoint handler"
  (let* ((env (getf params :env))
         (event-id (getf params :id))
         (org (getf env :organization))
         (org-id (getf org :id)))

    (unless org-id
      (return-from acknowledge-event-handler
        (zapier-triggers.utils:json-error-response
         "Organization not found"
         :status 401
         :code "unauthorized")))

    (unless event-id
      (return-from acknowledge-event-handler
        (zapier-triggers.utils:json-error-response
         "Event ID required"
         :status 400
         :code "missing_event_id")))

    ;; Check if event belongs to organization
    (unless (zapier-triggers.models:event-belongs-to-org-p event-id org-id)
      (return-from acknowledge-event-handler
        (zapier-triggers.utils:json-error-response
         "Event not found"
         :status 404
         :code "event_not_found")))

    ;; Update event status
    (if (zapier-triggers.models:update-event-status event-id "delivered")
        (list 204 '(:content-type "text/plain") '(""))
        (zapier-triggers.utils:json-error-response
         "Failed to acknowledge event"
         :status 500
         :code "update_failed"))))
