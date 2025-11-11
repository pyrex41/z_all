;;;; db/queries.lisp

(in-package #:zapier-triggers.db)

;;; Organization queries

(defun insert-organization (name api-key tier)
  "Insert a new organization into the database"
  (postmodern:query
   "INSERT INTO organizations (name, api_key, tier) VALUES ($1, $2, $3) RETURNING id"
   name api-key tier
   :single))

(defun find-organization-by-api-key (api-key)
  "Find organization by API key"
  (postmodern:query
   "SELECT id, name, api_key, tier, created_at FROM organizations WHERE api_key = $1"
   api-key
   :row))

(defun find-organization-by-id (org-id)
  "Find organization by ID"
  (postmodern:query
   "SELECT id, name, api_key, tier, created_at FROM organizations WHERE id = $1"
   org-id
   :row))

;;; Event queries

(defun insert-event (event-id org-id event-type payload &optional dedup-id)
  "Insert a new event"
  (postmodern:query
   "INSERT INTO events (id, organization_id, event_type, payload, dedup_id, status)
    VALUES ($1, $2, $3, $4, $5, 'pending')
    RETURNING id"
   event-id org-id event-type payload dedup-id
   :single))

(defun find-events-by-organization (org-id &key status limit offset)
  "Find events for an organization with optional filtering"
  (let ((query "SELECT id, organization_id, event_type, payload, status, created_at, delivered_at
                FROM events WHERE organization_id = $1")
        (params (list org-id)))
    ;; Add status filter if provided
    (when status
      (setf query (concatenate 'string query " AND status = $2"))
      (push status params))
    ;; Add ordering
    (setf query (concatenate 'string query " ORDER BY created_at DESC"))
    ;; Add limit
    (when limit
      (setf query (concatenate 'string query
                              (format nil " LIMIT $~D" (1+ (length params)))))
      (push limit params))
    ;; Add offset
    (when offset
      (setf query (concatenate 'string query
                              (format nil " OFFSET $~D" (1+ (length params)))))
      (push offset params))
    ;; Execute query with parameters in correct order
    (eval `(postmodern:query ,query ,@(reverse params)))))

(defun find-event-by-id (event-id)
  "Find event by ID"
  (postmodern:query
   "SELECT id, organization_id, event_type, payload, status, created_at, delivered_at
    FROM events WHERE id = $1"
   event-id
   :row))

(defun update-event-status (event-id status)
  "Update event status"
  (postmodern:query
   "UPDATE events SET status = $2, delivered_at = CASE WHEN $2 = 'delivered' THEN CURRENT_TIMESTAMP ELSE delivered_at END
    WHERE id = $1"
   event-id status
   :none)
  t)

(defun count-events-by-organization (org-id &optional status)
  "Count events for an organization"
  (if status
      (postmodern:query
       "SELECT COUNT(*) FROM events WHERE organization_id = $1 AND status = $2"
       org-id status
       :single)
      (postmodern:query
       "SELECT COUNT(*) FROM events WHERE organization_id = $1"
       org-id
       :single)))

;;; Webhook queries

(defun insert-webhook (org-id url &key auth-headers retry-count timeout)
  "Insert a new webhook configuration"
  (postmodern:query
   "INSERT INTO webhooks (organization_id, url, auth_headers, retry_count, timeout_seconds)
    VALUES ($1, $2, $3, $4, $5) RETURNING id"
   org-id url auth-headers (or retry-count 3) (or timeout 30)
   :single))

(defun find-webhook-by-organization (org-id)
  "Find webhook configuration for an organization"
  (postmodern:query
   "SELECT id, organization_id, url, auth_headers, retry_count, timeout_seconds, created_at
    FROM webhooks WHERE organization_id = $1 ORDER BY created_at DESC LIMIT 1"
   org-id
   :row))

(defun update-webhook (org-id url &key auth-headers retry-count timeout)
  "Update webhook configuration"
  (postmodern:query
   "UPDATE webhooks
    SET url = $2,
        auth_headers = $3,
        retry_count = $4,
        timeout_seconds = $5
    WHERE organization_id = $1"
   org-id url auth-headers (or retry-count 3) (or timeout 30)
   :none)
  t)
