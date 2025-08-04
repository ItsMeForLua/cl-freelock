(in-package #:cl-freelock-tests)

(def-suite spsc-queue-suite
  :description "Tests for the SPSC lock-free queue."
  :in cl-freelock-suite)

(in-suite spsc-queue-suite)

(test spsc-queue-creation
  "Tests the creation of SPSC queues."
  (is (make-spsc-queue 1024))
  (signals error (make-spsc-queue 999) "Should signal error for non-power-of-two capacity."))

(test basic-spsc-queue-operations
  "Tests single-threaded push and pop on the SPSC queue."
  (let ((q (make-spsc-queue 8)))
    (is (null (nth-value 1 (spsc-pop q))))
    (is-true (spsc-push q :item1))
    (multiple-value-bind (obj success) (spsc-pop q)
      (is (eq :item1 obj))
      (is-true success))
    (is (null (nth-value 1 (spsc-pop q))))))

(test spsc-queue-full-logic
  "Tests the full logic of the SPSC queue."
  (let* ((capacity 4)
         (q (make-spsc-queue capacity)))
    (dotimes (i capacity)
      (is-true (spsc-push q i)))
    (is-false (spsc-push q :should-fail) "Push should fail when queue is full.")
    (multiple-value-bind (obj success) (spsc-pop q)
      (is (= 0 obj))
      (is-true success))
    (is-true (spsc-push q :new-item) "Push should succeed after one item is popped.")))

(test single-producer-single-consumer
  "Tests the core SPSC use case with one producer and one consumer thread."
  (let* ((capacity 2048)
         (q (make-spsc-queue capacity))
         (num-items 100000)
         (results (make-array num-items :initial-element nil)))

    (let ((consumer (make-thread
                     (lambda ()
                       (dotimes (i num-items)
                         (loop
                           (multiple-value-bind (obj success) (spsc-pop q)
                             (when success
                               (setf (aref results obj) t)
                               (return))))))
                     :name "SPSC Consumer"))
          (producer (make-thread
                     (lambda ()
                       (dotimes (i num-items)
                         (loop until (spsc-push q i)))
                       )
                     :name "SPSC Producer")))
      (join-thread producer)
      (join-thread consumer))

    (is (every #'identity results) "All items should be pushed and popped correctly.")))
