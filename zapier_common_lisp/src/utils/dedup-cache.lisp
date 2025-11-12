;;;; utils/dedup-cache.lisp
;;;; In-memory deduplication cache for fast duplicate detection

(in-package #:zapier-triggers.utils)

;;; Thread-safe in-memory cache for dedup IDs
(defvar *dedup-cache* (make-hash-table :test 'equal)
  "In-memory cache of dedup IDs (org-id:dedup-id -> t)")

(defvar *dedup-cache-lock* (bt:make-lock "dedup-cache-lock")
  "Lock for thread-safe dedup cache operations")

(defvar *dedup-cache-max-size* 100000
  "Maximum number of entries in dedup cache before cleanup")

(defun make-dedup-cache-key (org-id dedup-id)
  "Create cache key from org-id and dedup-id"
  (format nil "~A:~A" org-id dedup-id))

(defun dedup-cache-check (org-id dedup-id)
  "Check if dedup-id exists in cache. Returns t if duplicate, nil if new.
   This is a fast O(1) memory lookup."
  (bt:with-lock-held (*dedup-cache-lock*)
    (let ((key (make-dedup-cache-key org-id dedup-id)))
      (gethash key *dedup-cache*))))

(defun dedup-cache-add (org-id dedup-id)
  "Add dedup-id to cache. Returns t if added, nil if already existed."
  (bt:with-lock-held (*dedup-cache-lock*)
    (let ((key (make-dedup-cache-key org-id dedup-id)))
      (if (gethash key *dedup-cache*)
          nil  ; Already exists
          (progn
            ;; Check cache size and cleanup if needed
            (when (> (hash-table-count *dedup-cache*) *dedup-cache-max-size*)
              (format t "~&[DEDUP-CACHE] Max size reached, clearing cache~%")
              (clrhash *dedup-cache*))
            (setf (gethash key *dedup-cache*) t)
            t)))))

(defun dedup-cache-clear ()
  "Clear all entries from dedup cache"
  (bt:with-lock-held (*dedup-cache-lock*)
    (clrhash *dedup-cache*)))

(defun dedup-cache-stats ()
  "Get dedup cache statistics"
  (bt:with-lock-held (*dedup-cache-lock*)
    (list :size (hash-table-count *dedup-cache*)
          :max-size *dedup-cache-max-size*)))
