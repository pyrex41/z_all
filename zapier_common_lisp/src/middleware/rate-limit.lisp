;;;; middleware/rate-limit.lisp

(in-package #:zapier-triggers.middleware)

;;; Rate limit configuration per tier
(defparameter *rate-limits*
  '(("free" . 10)           ; 10 requests per minute
    ("starter" . 60)         ; 60 requests per minute
    ("professional" . 600)   ; 600 requests per minute
    ("enterprise" . 6000))   ; 6000 requests per minute
  "Rate limits by tier (requests per minute)")

;;; Token bucket storage (org-id -> bucket)
(defvar *rate-limit-buckets* (make-hash-table :test 'equal)
  "Hash table storing token buckets for each organization")

(defvar *rate-limit-lock* (bt:make-lock "rate-limit-lock")
  "Lock for thread-safe rate limit operations")

(defstruct token-bucket
  (tokens 0 :type integer)
  (capacity 0 :type integer)
  (last-refill (get-universal-time) :type integer)
  (refill-rate 0 :type number))

(defun get-rate-limit (tier)
  "Get rate limit for tier"
  (or (cdr (assoc tier *rate-limits* :test #'string=)) 10))

(defun create-bucket (tier)
  "Create a new token bucket for tier"
  (let ((capacity (get-rate-limit tier)))
    (make-token-bucket
     :tokens capacity
     :capacity capacity
     :last-refill (get-universal-time)
     :refill-rate (/ capacity 60.0)))) ; tokens per second

(defun refill-bucket (bucket)
  "Refill bucket based on time elapsed"
  (let* ((now (get-universal-time))
         (elapsed (- now (token-bucket-last-refill bucket)))
         (tokens-to-add (floor (* elapsed (token-bucket-refill-rate bucket))))
         (new-tokens (min (token-bucket-capacity bucket)
                         (+ (token-bucket-tokens bucket) tokens-to-add))))
    (setf (token-bucket-tokens bucket) new-tokens)
    (setf (token-bucket-last-refill bucket) now)
    bucket))

(defun consume-token (bucket)
  "Try to consume a token from bucket, return t if successful"
  (refill-bucket bucket)
  (if (> (token-bucket-tokens bucket) 0)
      (progn
        (decf (token-bucket-tokens bucket))
        t)
      nil))

(defun get-or-create-bucket (org-id tier)
  "Get or create token bucket for organization"
  (bt:with-lock-held (*rate-limit-lock*)
    (or (gethash org-id *rate-limit-buckets*)
        (setf (gethash org-id *rate-limit-buckets*)
              (create-bucket tier)))))

(defun within-limit-p (org-id tier)
  "Check if request is within rate limit"
  (let ((bucket (get-or-create-bucket org-id tier)))
    (bt:with-lock-held (*rate-limit-lock*)
      (consume-token bucket))))

(defun rate-limit-middleware (app)
  "Middleware to enforce rate limiting"
  (lambda (env)
    (let ((path (getf env :path-info)))
      ;; Skip rate limiting for health check and key generation
      (if (or (string= path "/health")
              (string= path "/api/keys/generate"))
          (funcall app env)
          ;; Check rate limit for other endpoints
          (let* ((org (getf env :organization))
                 (org-id (when org (getf org :id)))
                 (tier (when org (getf org :tier))))
            (if (and org-id tier (within-limit-p org-id tier))
                (funcall app env)
                ;; Rate limit exceeded
                (zapier-triggers.utils:json-error-response
                 "Rate limit exceeded"
                 :status 429
                 :code "rate_limit_exceeded")))))))

(defun reset-rate-limits ()
  "Reset all rate limit buckets (for testing)"
  (bt:with-lock-held (*rate-limit-lock*)
    (clrhash *rate-limit-buckets*)))
