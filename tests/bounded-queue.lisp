(in-package #:cl-freelock-tests)

(def-suite bounded-queue-suite
  :description "Tests for the bounded lock-free queue."
  :in cl-freelock-suite)

(in-suite bounded-queue-suite)

(test bounded-queue-creation
  "Tests the creation of bounded queues."
  (is (make-bounded-queue 1024))
  (signals error (make-bounded-queue 1000) "Should signal error for non-power-of-two capacity."))

(test basic-bounded-queue-operations
  "Tests single-threaded push and pop on the bounded queue."
  (let ((q (make-bounded-queue 8)))
    (is (bounded-queue-empty-p q))
    (is-false (bounded-queue-full-p q))
    (is (= 8 (bounded-queue-capacity q)))
    (is-true (bounded-queue-push q :item1))
    (is-false (bounded-queue-empty-p q))
    (multiple-value-bind (obj success) (bounded-queue-pop q)
      (is (eq :item1 obj))
      (is-true success))
    (is (bounded-queue-empty-p q))
    (multiple-value-bind (obj success) (bounded-queue-pop q)
      (is (null obj))
      (is-false success))))

(test bounded-queue-full-and-empty
  "Tests the full and empty logic of the bounded queue."
  (let* ((capacity 4)
         (q (make-bounded-queue capacity)))
    (dotimes (i capacity)
      (is-true (bounded-queue-push q i)))
    (is-true (bounded-queue-full-p q))
    (is-false (bounded-queue-empty-p q))
    (is-false (bounded-queue-push q :should-fail))
    (dotimes (i capacity)
      (multiple-value-bind (obj success) (bounded-queue-pop q)
        (is (= i obj))
        (is-true success)))
    (is-true (bounded-queue-empty-p q))
    (is-false (bounded-queue-full-p q))))

(test multi-threaded-bounded-queue
  "Tests concurrent push and pop on the bounded queue."
  (let* ((capacity 1024)
         (q (make-bounded-queue capacity))
         (num-items 10000)
         (num-producers 4)
         (num-consumers 4)
         (items-per-producer (floor num-items num-producers))
         (results (make-array num-items :initial-element nil))
         (items-popped (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil))
    (dotimes (i num-consumers)
      (push (make-thread
             (lambda ()
               (loop
                 (when (>= (atomic-ref-value items-popped) num-items) (return))
                 (multiple-value-bind (obj success) (bounded-queue-pop q)
                   (when success
                     (setf (aref results obj) t)
                     (atomic-incf items-popped)))
                   (thread-yield)))
             :name (format nil "Consumer ~D" i))
            consumers))
    (dotimes (i num-producers)
      (let ((start-item (* i items-per-producer))
            (end-item (if (= i (1- num-producers))
                          num-items
                          (* (1+ i) items-per-producer))))
        (push (make-thread
               (lambda ()
                 (loop for item from start-item below end-item do
                   (loop until (bounded-queue-push q item) do (thread-yield))))
               :name (format nil "Producer ~D" i))
              producers)))
    (mapc #'join-thread producers)
    (mapc #'join-thread consumers)
    (let ((missing-items (loop for i below num-items unless (aref results i) collect i)))
      (is (null missing-items) "Missing items: ~A" missing-items))))

(test bounded-queue-batch-operations
  "Tests the batch push and pop operations."
  (let* ((capacity 8)
         (q (make-bounded-queue capacity))
         (batch1 (list 0 1 2 3))
         (batch2 (list 4 5 6 7))
         (too-big-batch (list 0 1 2 3 4 5 6 7 8)))
    (is-true (bounded-queue-push-batch q batch1))
    (is-false (bounded-queue-empty-p q))
    (is-false (bounded-queue-full-p q))
    (is-true (bounded-queue-push-batch q batch2))
    (is-true (bounded-queue-full-p q))
    (is-false (bounded-queue-push-batch q '(9)) "Should not be able to push to a full queue.")
    (is-false (bounded-queue-push-batch q too-big-batch) "Should not be able to push a batch larger than capacity.")
    (multiple-value-bind (popped success) (bounded-queue-pop-batch q capacity)
      (is-true success)
      (is (equal batch1 (subseq popped 0 4)))
      (is (equal batch2 (subseq popped 4 8))))
    (is (bounded-queue-empty-p q))
    (multiple-value-bind (popped success) (bounded-queue-pop-batch q 1)
      (is-false success)
      (is (null popped)))))
