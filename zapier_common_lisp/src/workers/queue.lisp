;;;; workers/queue.lisp
;;;; Async event processing queue using lparallel

(in-package #:zapier-triggers)

(defvar *event-queue* nil
  "In-memory event processing queue")

(defvar *worker-threads* nil
  "List of background worker threads")

(defvar *queue-lock* (bt:make-lock "queue-lock")
  "Lock for thread-safe queue operations")

(defvar *processing-lock* (bt:make-lock "processing-lock")
  "Lock for processing operations")

(defstruct queued-event
  "Event queued for async processing"
  id
  org-id
  event-type
  payload
  dedup-id
  timestamp)

(defun init-queue ()
  "Initialize event processing queue"
  (bt:with-lock-held (*queue-lock*)
    (unless *event-queue*
      (setf *event-queue* (make-array 10000 :adjustable t :fill-pointer 0))
      (format t "~&[QUEUE] Event queue initialized~%"))))

(defun enqueue-event (org-id event-type payload &optional dedup-id)
  "Add event to processing queue (non-blocking)"
  (bt:with-lock-held (*queue-lock*)
    (let ((event (make-queued-event
                  :id (zapier-triggers.utils:generate-event-id)
                  :org-id org-id
                  :event-type event-type
                  :payload payload
                  :dedup-id dedup-id
                  :timestamp (get-universal-time))))
      (vector-push-extend event *event-queue*)
      (queued-event-id event))))

(defun dequeue-event ()
  "Get next event from queue (thread-safe)"
  (bt:with-lock-held (*queue-lock*)
    (when (> (fill-pointer *event-queue*) 0)
      (let ((event (aref *event-queue* 0)))
        ;; Shift array left
        (loop for i from 0 below (1- (fill-pointer *event-queue*))
              do (setf (aref *event-queue* i)
                      (aref *event-queue* (1+ i))))
        (decf (fill-pointer *event-queue*))
        event))))

(defun queue-depth ()
  "Get current queue depth"
  (bt:with-lock-held (*queue-lock*)
    (fill-pointer *event-queue*)))

(defun process-queued-event (event)
  "Process a single queued event (background worker)"
  (handler-case
      (bt:with-lock-held (*processing-lock*)
        (zapier-triggers.db:with-connection
          (let ((event-id (queued-event-id event))
                (org-id (queued-event-org-id event))
                (event-type (queued-event-event-type event))
                (payload (queued-event-payload event))
                (dedup-id (queued-event-dedup-id event)))

            ;; Convert payload to JSON string
            (let ((payload-json (jonathan:to-json payload :from :plist)))

              ;; Check for duplicate if dedup-id provided
              (when dedup-id
                (let ((existing (postmodern:query
                                "SELECT id FROM events
                                 WHERE organization_id = $1 AND dedup_id = $2"
                                org-id dedup-id
                                :single)))
                  (when existing
                    (format t "~&[QUEUE] Duplicate event skipped: ~A~%" dedup-id)
                    (return-from process-queued-event nil))))

              ;; Insert event into database
              (zapier-triggers.db::insert-event event-id org-id event-type
                                                payload-json dedup-id)

              (format t "~&[QUEUE] Event processed: ~A~%" event-id)
              t))))
    (error (e)
      (format t "~&[QUEUE ERROR] Failed to process event: ~A~%" e)
      nil)))

(defun worker-loop ()
  "Background worker loop - processes events from queue"
  (loop
    (let ((event (dequeue-event)))
      (if event
          (process-queued-event event)
          (sleep 0.01))) ; Short sleep if queue is empty
    (when (not *worker-threads*)
      (return)))) ; Exit if workers are stopped

(defun start-workers (&optional (num-workers 2))
  "Start background worker threads"
  (unless *worker-threads*
    (init-queue)
    (setf *worker-threads*
          (loop for i from 1 to num-workers
                collect (bt:make-thread
                        #'worker-loop
                        :name (format nil "event-worker-~D" i))))
    (format t "~&[WORKERS] Started ~D background workers~%" num-workers)))

(defun stop-workers ()
  "Stop background worker threads"
  (when *worker-threads*
    (format t "~&[WORKERS] Stopping workers...~%")
    ;; Signal workers to stop
    (setf *worker-threads* nil)
    (sleep 1) ; Give workers time to exit
    (format t "~&[WORKERS] Workers stopped~%")))

(defun queue-stats ()
  "Get queue statistics"
  (list :depth (queue-depth)
        :workers (length *worker-threads*)
        :timestamp (local-time:now)))
