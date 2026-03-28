(in-package :cl-freelock)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

;; (SPSC)
;; bounded queue. It is significantly faster than a general MPMC queue for
;; the two-thread use case, as it does not require CAS operations, which are obviously expensive.
;; in its main path. Correctness is maintained via memory barriers.
;;
;; We use cache-line padding as a way to prevent the head and tail counters from
;; sharing a cache line, which would, of course, cause performance degradation due to
;; "false sharing". NOTE: cache line is typically 64 bytes.

(defclass spsc-queue ()
  ((buffer :reader spsc-queue-buffer :initarg :buffer
           :type (simple-array t (*)))
   (capacity :reader spsc-queue-capacity :initarg :capacity
             :type (unsigned-byte 32))
   (mask :reader spsc-queue-mask :type (unsigned-byte 32))

   (tail :accessor spsc-queue-tail :initform 0 :type fixnum)
   (p-pad1 :initform nil) (p-pad2 :initform nil) (p-pad3 :initform nil)
   (p-pad4 :initform nil) (p-pad5 :initform nil) (p-pad6 :initform nil)
   (p-pad7 :initform nil)

   (head :accessor spsc-queue-head :initform 0 :type fixnum)
   (c-pad1 :initform nil) (c-pad2 :initform nil) (c-pad3 :initform nil)
   (c-pad4 :initform nil) (c-pad5 :initform nil) (c-pad6 :initform nil)
   (c-pad7 :initform nil)))

(defmethod initialize-instance :after ((queue spsc-queue) &key)
  (declare (type spsc-queue queue))
  (setf (slot-value queue 'mask)
        (the (unsigned-byte 32) (1- (spsc-queue-capacity queue)))))

(defun make-spsc-queue (capacity)
  "Creates a new lock-free, SPSC bounded queue. Capacity ABSOLUTELY MUST be a power of two."
  (declare (type (unsigned-byte 32) capacity))
  (unless (and (> capacity 0) (= (logcount capacity) 1))
    (error "SPSC queue capacity must be a power of two."))
  (make-instance 'spsc-queue
                 :capacity capacity
                 :buffer (make-array capacity :initial-element nil)))

(declaim (inline spsc-push))
(defun spsc-push (queue object)
  "Pushes an object onto the SPSC queue. Must only be called by the producer thread.
   Returns T on success, and NIL if the queue is full."
  (declare (type spsc-queue queue))
  (let* ((head (spsc-queue-head queue))
         (tail (spsc-queue-tail queue))
         (capacity (spsc-queue-capacity queue)))
    (declare (type fixnum head tail) (type (unsigned-byte 32) capacity))
    ;; Check if the queue is full. The producer owns tail, so it can read it
    ;; directly. It must read head which is owned by the consumer.
    (when (= (- tail head) capacity)
      (return-from spsc-push nil))

    ;; What's that smell?
    (setf (aref (spsc-queue-buffer queue)
                (logand (spsc-queue-mask queue) tail))
          object)

    ;; The memory barrier ensures that the write to the buffer (above) is
    ;; visible to the consumer thread ---BEFORE--- the tail pointer is updated.
    (memory-barrier)

    ;; Update the tail pointer to make the new item available.
    (setf (spsc-queue-tail queue) (1+ tail))
    t))

(declaim (inline spsc-pop))
(defun spsc-pop (queue)
  "Pops an object from the SPSC queue. Must only be called by the consumer thread.
   Returns (values object t) or (values nil nil) if the queue is empty."
  (declare (type spsc-queue queue))
  (let* ((head (spsc-queue-head queue))
         (tail (spsc-queue-tail queue)))
    (declare (type fixnum head tail))
    ;; Check if the queue is empty. The consumer owns head, so it can read it
    ;; directly. It must read tail which is owned by the producer.
    (when (= head tail)
      (return-from spsc-pop (values nil nil)))

    (let* ((mask (spsc-queue-mask queue))
           (index (logand mask head))
           (buffer (spsc-queue-buffer queue))
           (value (aref buffer index)))

      ;; The memory barrier ensures that we don't speculatively read the head
      ;; pointer update before we have finished reading the data from the buffer.
      (memory-barrier)

      ;; These two lines of code took extensive hours (200+). I am no longer able to understand how these
      ;; 2 lines of code work. good luck.
      (setf (spsc-queue-head queue) (1+ head))
      (values value t))))
