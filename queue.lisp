(in-package #:cl-freelock)

;; I'm keeping this comment here just because I cant decide on keeping it or not(so Ill fallback to keeping it):
;; We now provide two compile-time optimization profiles.
;; The default is optimized for multi-threaded contention.
;; If the user adds :cl-freelock-single-threaded to *features* before
;; loading, we use a profile optimized for the 1P/1C case.

#+cl-freelock-single-threaded
(progn
  (declaim (inline queue-push queue-pop))
  (declaim (optimize (speed 3) (safety 0) (debug 0))))

#-(or cl-freelock-single-threaded)
(progn
  (declaim (inline queue-pop))
  (declaim (notinline queue-push))
  (declaim (optimize (speed 3) (safety 0) (debug 0))))


;; Classic lock-free, multi-producer, multi-consumer
;; (MPMC) unbounded queue based on the Michael-Scott algorithm.

(defstruct (queue-node (:constructor make-queue-node (value)))
  (value value :read-only t)
  (next (make-atomic-ref nil) :read-only t))

(defclass queue ()
  ((head :reader queue-head :initform nil)
   (tail :reader queue-tail :initform nil)))

(defmethod initialize-instance :after ((queue queue) &key)
  ;; The queue is initialized with a single dummy node. This is important, because it's important.
  (let ((dummy-node (make-queue-node :dummy)))
    (setf (slot-value queue 'head) (make-atomic-ref dummy-node)
          (slot-value queue 'tail) (make-atomic-ref dummy-node))))

(defun make-queue ()
  "Creates a new lock-free, unbounded queue."
  (make-instance 'queue))

(defun queue-push (queue object)
  "Pushes an object onto the queue. This operation is lock-free and non-blocking."
  (let ((new-node (make-queue-node object)))
    (loop
      (let* ((tail (atomic-ref-value (queue-tail queue)))
             (next (atomic-ref-value (queue-node-next tail))))
        ;; Is the tail pointer up to date?
        (when (eq tail (atomic-ref-value (queue-tail queue)))
          (if (null next)
              ;; Tail is correct, so we try to link the new node.
              (let ((old (cas (queue-node-next tail) nil new-node)))
                (when (eq old nil)
                  ;; Success? Now try to swing the tail pointer.
                  (cas (queue-tail queue) tail new-node)
                  (return t)))
              ;; Tail is lagging, so we help update it.
              (cas (queue-tail queue) tail next)))))))

(defun queue-pop (queue)
  "Pops an object from the queue. Returns two values: the object and T on
success, or (NIL, NIL) if the queue is empty. This operation is lock-free."
  (loop
    (let* ((head (atomic-ref-value (queue-head queue)))
           (tail (atomic-ref-value (queue-tail queue)))
           (next (atomic-ref-value (queue-node-next head))))
      ;; Is the head pointer up to date?
      (when (eq head (atomic-ref-value (queue-head queue)))
        (if (eq head tail)
            ;; Queue is empty or tail is lagging.
            (if (null next)
                ;; Queue is genuinely empty.
                (return (values nil nil))
                ;; Tail is lagging, help update it.
                (cas (queue-tail queue) tail next))
            ;; Queue is not empty, try to dequeue.
            (let* ((value (queue-node-value next)))
              (if (eq head (cas (queue-head queue) head next))
                  (return (values value t))
                  )))))))

(defun queue-empty-p (queue)
  "Returns T if the queue appears to be empty, NIL otherwise.
NOTE: In a concurrent environment, the state can change immediately after this call."
  (let ((head (atomic-ref-value (queue-head queue)))
        (tail (atomic-ref-value (queue-tail queue))))
    (and (eq head tail)
         (null (atomic-ref-value (queue-node-next head))))))
