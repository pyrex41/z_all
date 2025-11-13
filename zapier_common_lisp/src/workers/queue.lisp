;;;; workers/queue.lisp
;;;; High-performance async event processing queue with circular buffer

(in-package #:zapier-triggers)

;;; Circular buffer queue for O(1) enqueue/dequeue
(defstruct queue-state
  "Circular buffer queue state"
  (buffer (make-array 10000 :initial-element nil) :type simple-vector)
  (capacity 10000 :type fixnum)
  (head 0 :type fixnum)  ; Read position
  (tail 0 :type fixnum)  ; Write position
  (size 0 :type fixnum)) ; Current size

(defvar *event-queue* nil
  "In-memory event processing queue (circular buffer)")

(defvar *worker-threads* nil
  "List of background worker threads")

(defvar *queue-lock* (bt:make-lock "queue-lock")
  "Lock for thread-safe queue operations")

(defvar *queue-cv* (bt:make-condition-variable)
  "Condition variable for event-driven queue processing")

(defstruct queued-event
  "Event queued for async processing"
  id
  org-id
  event-type
  payload
  dedup-id
  timestamp)

(defun init-queue ()
  "Initialize event processing queue with circular buffer"
  (bt:with-lock-held (*queue-lock*)
    (unless *event-queue*
      (setf *event-queue* (make-queue-state))
      (format t "~&[QUEUE] High-performance circular buffer queue initialized~%"))))

(defun queue-full-p (q)
  "Check if queue is full"
  (declare (type queue-state q))
  (= (queue-state-size q) (queue-state-capacity q)))

(defun queue-empty-p (q)
  "Check if queue is empty"
  (declare (type queue-state q))
  (zerop (queue-state-size q)))

(defun enqueue-event (org-id event-type payload &optional dedup-id)
  "Add event to processing queue - O(1) operation with condition variable notification"
  (let ((event (make-queued-event
                :id (zapier-triggers.utils:generate-event-id)
                :org-id org-id
                :event-type event-type
                :payload payload
                :dedup-id dedup-id
                :timestamp (get-universal-time))))
    (bt:with-lock-held (*queue-lock*)
      (when (queue-full-p *event-queue*)
        (format t "~&[QUEUE WARNING] Queue full, dropping event~%")
        (return-from enqueue-event nil))

      ;; Add to circular buffer - O(1)
      (let* ((q *event-queue*)
             (tail (queue-state-tail q))
             (buffer (queue-state-buffer q)))
        (setf (svref buffer tail) event)
        (setf (queue-state-tail q) (mod (1+ tail) (queue-state-capacity q)))
        (incf (queue-state-size q))

        ;; Signal waiting workers via condition variable
        (bt:condition-notify *queue-cv*)

        (queued-event-id event)))))

(defun dequeue-event ()
  "Get next event from queue - O(1) operation"
  (bt:with-lock-held (*queue-lock*)
    (when (queue-empty-p *event-queue*)
      (return-from dequeue-event nil))

    ;; Remove from circular buffer - O(1)
    (let* ((q *event-queue*)
           (head (queue-state-head q))
           (buffer (queue-state-buffer q))
           (event (svref buffer head)))
      (setf (svref buffer head) nil) ; Clear reference
      (setf (queue-state-head q) (mod (1+ head) (queue-state-capacity q)))
      (decf (queue-state-size q))
      event)))

(defun queue-depth ()
  "Get current queue depth"
  (bt:with-lock-held (*queue-lock*)
    (if *event-queue*
        (queue-state-size *event-queue*)
        0)))

(defun process-queued-event (event)
  "Process a single queued event - NO GLOBAL LOCK for parallel processing"
  (handler-case
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
                  ;; Return :duplicate to signal duplicate detected
                  (return-from process-queued-event :duplicate))))

            ;; Insert event into database
            (zapier-triggers.db::insert-event event-id org-id event-type
                                              payload-json dedup-id)

            ;; Add to dedup cache for future fast lookups
            (when dedup-id
              (zapier-triggers.utils:dedup-cache-add org-id dedup-id))

            (format t "~&[QUEUE] Event processed: ~A~%" event-id)
            t)))
    (error (e)
      (format t "~&[QUEUE ERROR] Failed to process event: ~A~%" e)
      nil)))

(defun worker-loop ()
  "Background worker loop - event-driven with condition variables (no polling!)"
  (loop
    (let ((event nil))
      ;; Wait for event with condition variable - blocks until signaled
      (bt:with-lock-held (*queue-lock*)
        (loop while (and *worker-threads* (queue-empty-p *event-queue*))
              do (bt:condition-wait *queue-cv* *queue-lock*))

        ;; Check if we should exit
        (unless *worker-threads*
          (return-from worker-loop))

        ;; Dequeue inline to avoid recursive lock (dequeue-event also takes lock)
        (unless (queue-empty-p *event-queue*)
          (let* ((q *event-queue*)
                 (head (queue-state-head q))
                 (buffer (queue-state-buffer q)))
            (setf event (svref buffer head))
            (setf (svref buffer head) nil)
            (setf (queue-state-head q) (mod (1+ head) (queue-state-capacity q)))
            (decf (queue-state-size q)))))

      ;; Process event outside lock for parallel processing
      (when event
        (process-queued-event event)))))

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
  "Stop background worker threads gracefully"
  (when *worker-threads*
    (format t "~&[WORKERS] Stopping workers...~%")
    ;; Signal workers to stop and wake them up
    (bt:with-lock-held (*queue-lock*)
      (setf *worker-threads* nil)
      ;; Broadcast to all waiting workers
      (bt:condition-notify *queue-cv*))
    (sleep 0.5) ; Give workers time to exit
    (format t "~&[WORKERS] Workers stopped~%")))

(defun queue-stats ()
  "Get queue statistics"
  (list :depth (queue-depth)
        :workers (length *worker-threads*)
        :timestamp (local-time:now)))
