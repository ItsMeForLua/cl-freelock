;;;; bounded-queue.lisp Vyukov MPMC Bounded Queue

(in-package :cl-freelock)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

;; This file implements a lock-free, multi-producer, multi-consumer (MPMC)
;; bounded queue based on the algorithm described by Dmitry Vyukov.
;; It is immune to livelock, which is why we use it for bounded queue over a michael scott variant.

(defclass bounded-queue ()
  ((buffer :reader bounded-queue-buffer :initarg :buffer
           :type (simple-array t (*)))
   (sequences :reader bounded-queue-sequences :initarg :sequences
              :type (simple-array t (*)))
   (capacity :reader bounded-queue-capacity :initarg :capacity
             :type (unsigned-byte 32))
   (mask :reader bounded-queue-mask :type (unsigned-byte 32))
   ;; Head/Tail counters track the total number of pops/pushes initiated.
   (head :reader bounded-queue-head :initform (make-atomic-ref 0))
   (tail :reader bounded-queue-tail :initform (make-atomic-ref 0))))

(defmethod initialize-instance :after ((queue bounded-queue) &key)
  (declare (type bounded-queue queue))
  (setf (slot-value queue 'mask)
        (the (unsigned-byte 32) (1- (bounded-queue-capacity queue))))
  ;; Initialize the sequence array. Slot `i` is initially ready for push `i`.
  (let ((sequences (bounded-queue-sequences queue)))
    (declare (type (simple-array t (*)) sequences))
    (dotimes (i (bounded-queue-capacity queue))
      (setf (aref sequences i) (make-atomic-ref (the fixnum i))))))

(defun make-bounded-queue (capacity)
  "Creates a new lock-free, bounded queue. Capacity MUST be a power of two."
  (declare (type (unsigned-byte 32) capacity))
  (unless (and (> capacity 0) (= (logcount capacity) 1))
    (error "Bounded queue capacity must be a power of two."))
  (make-instance 'bounded-queue
                 :capacity capacity
                 :buffer (make-array capacity :initial-element nil)
                 ;; Provide a valid initial-element to satisfy the compiler.
                 :sequences (make-array capacity :initial-element nil)))

(declaim (inline bounded-queue-push))
(defun bounded-queue-push (queue object)
  "Pushes an object onto the bounded queue. Returns T on success, NIL if full.
   This operation is lock-free and non-blocking."
  (declare (type bounded-queue queue))
  (loop
    (let* ((tail (the fixnum (atomic-ref-value (bounded-queue-tail queue))))
           (head (the fixnum (atomic-ref-value (bounded-queue-head queue))))
           (mask (the (unsigned-byte 32) (bounded-queue-mask queue)))
           (index (the fixnum (logand mask tail)))
           (seq-ref (aref (bounded-queue-sequences queue) index)))
      (declare (type fixnum tail head index)
               (type atomic-ref seq-ref))

      (when (>= (- tail head) (the fixnum (bounded-queue-capacity queue)))
        (when (= head (the fixnum (atomic-ref-value (bounded-queue-head queue))))
          (return-from bounded-queue-push nil)))

      (when (= (the fixnum (atomic-ref-value seq-ref)) tail)
        (when (eq tail (cas (bounded-queue-tail queue) tail (the fixnum (1+ tail))))
          (setf (aref (bounded-queue-buffer queue) index) object)
          (setf (atomic-ref-value seq-ref) (the fixnum (1+ tail)))
          (return-from bounded-queue-push t)))
      (thread-yield))))

(declaim (inline bounded-queue-pop))
(defun bounded-queue-pop (queue)
  "Pops an object from the queue. Returns (values object t) or (values nil nil).
   This operation is lock-free and non-blocking."
  (declare (type bounded-queue queue))
  (loop
    (let* ((head (the fixnum (atomic-ref-value (bounded-queue-head queue))))
           (tail (the fixnum (atomic-ref-value (bounded-queue-tail queue))))
           (mask (the (unsigned-byte 32) (bounded-queue-mask queue)))
           (index (the fixnum (logand mask head)))
           (seq-ref (aref (bounded-queue-sequences queue) index)))
      (declare (type fixnum head tail index)
               (type atomic-ref seq-ref))

      (when (= head tail)
        (when (= tail (the fixnum (atomic-ref-value (bounded-queue-tail queue))))
          (return-from bounded-queue-pop (values nil nil))))

      (when (= (the fixnum (atomic-ref-value seq-ref)) (the fixnum (1+ head)))
        (when (eq head (cas (bounded-queue-head queue) head (the fixnum (1+ head))))
          (let ((value (aref (bounded-queue-buffer queue) index)))
            (setf (atomic-ref-value seq-ref)
                  (the fixnum (+ head (the fixnum (bounded-queue-capacity queue)))))
            (return-from bounded-queue-pop (values value t)))))
      (thread-yield))))

(defun bounded-queue-empty-p (queue)
  "Returns T if the queue appears to be empty, and NIL otherwise."
  (declare (type bounded-queue queue))
  (= (the fixnum (atomic-ref-value (bounded-queue-head queue)))
     (the fixnum (atomic-ref-value (bounded-queue-tail queue)))))

(defun bounded-queue-full-p (queue)
  "Returns T if the queue appears to be full, NIL otherwise."
  (declare (type bounded-queue queue))
  (>= (- (the fixnum (atomic-ref-value (bounded-queue-tail queue)))
         (the fixnum (atomic-ref-value (bounded-queue-head queue))))
      (the fixnum (bounded-queue-capacity queue))))

;;;
;;; Batch Operations
;;;

(defun bounded-queue-push-batch (queue sequence)
  "Pushes a sequence of objects onto the queue. Returns T on success, NIL if
   there is not enough space for the entire sequence."
  (declare (type bounded-queue queue) (type sequence sequence))
  (let ((batch-size (length sequence)))
    (loop
      (let* ((tail (the fixnum (atomic-ref-value (bounded-queue-tail queue))))
             (head (the fixnum (atomic-ref-value (bounded-queue-head queue)))))
        ;; Check if there is enough space for the whole batch.
        (when (> (+ (- tail head) batch-size) (bounded-queue-capacity queue))
          (return-from bounded-queue-push-batch nil))
        
        ;; Try to claim the block of slots.
        (when (eq tail (cas (bounded-queue-tail queue) tail (+ tail batch-size)))
          ;; Successfully claimed the block. Now we fill it.
          (dotimes (i batch-size)
            (let* ((current-tail (+ tail i))
                   (index (logand (bounded-queue-mask queue) current-tail))
                   (seq-ref (aref (bounded-queue-sequences queue) index)))
              ;; Wait for the slot to be ready for this specific push.
              (loop until (= (the fixnum (atomic-ref-value seq-ref)) current-tail))
              ;; Then write the data.
              (setf (aref (bounded-queue-buffer queue) index) (elt sequence i))
              ;; Publish the write for this slot.
              (setf (atomic-ref-value seq-ref) (1+ current-tail))))
          (return-from bounded-queue-push-batch t)))
      (thread-yield))))

(defun bounded-queue-pop-batch (queue count)
  "Pops up to COUNT objects from the queue. Returns two values:
   1. A (possibly empty) list of objects.
   2. T if any objects were popped, NIL otherwise."
  (declare (type bounded-queue queue) (type (integer 1 *) count))
  (let ((result (make-array count :fill-pointer 0)))
    (loop
      (let* ((head (the fixnum (atomic-ref-value (bounded-queue-head queue))))
             (tail (the fixnum (atomic-ref-value (bounded-queue-tail queue))))
             (available (- tail head))
             (batch-size (min count available)))
        
        (when (zerop batch-size)
          (return-from bounded-queue-pop-batch (values nil nil)))

        ;; Try to claim the block of slots.
        (when (eq head (cas (bounded-queue-head queue) head (+ head batch-size)))
          ;; Successfully claimed the block. Now drain it.
          (dotimes (i batch-size)
            (let* ((current-head (+ head i))
                   (index (logand (bounded-queue-mask queue) current-head))
                   (seq-ref (aref (bounded-queue-sequences queue) index)))
              ;; Wait for the slot to be ready for this specific pop.
              (loop until (= (the fixnum (atomic-ref-value seq-ref)) (1+ current-head)))
              ;; Read the data.
              (vector-push (aref (bounded-queue-buffer queue) index) result)
              ;; Mark the slot as empty for a future push.
              (setf (atomic-ref-value seq-ref)
                    (+ current-head (bounded-queue-capacity queue)))))
          (return-from bounded-queue-pop-batch (values (coerce result 'list) t))))
      (thread-yield))))
