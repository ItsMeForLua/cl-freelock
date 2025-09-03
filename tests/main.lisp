(defpackage #:cl-freelock-tests
  (:use #:cl #:fl #:fiveam #:bordeaux-threads)
  (:export #:run-tests))

(in-package :cl-freelock-tests)

(def-suite cl-freelock-suite
  :description "Test suite for the cl-freelock library.")

(in-suite cl-freelock-suite)

(test basic-queue-operations
  "Tests basic single-threaded operations on the queue."
  (let ((q (make-queue)))
    (is (queue-empty-p q))
    
    (is-true (queue-push q :item1))
    (is-false (queue-empty-p q))

    (multiple-value-bind (obj success) (queue-pop q)
      (is (eq :item1 obj))
      (is-true success))
    (is (queue-empty-p q))

    (multiple-value-bind (obj success) (queue-pop q)
      (is (null obj))
      (is-false success))))

(test multi-threaded-queue
  "Tests concurrent push and pop operations."
  (let* ((q (make-queue))
         (num-items 10000)
         (results (make-array num-items :initial-element nil))
         (producer (make-thread
                     (lambda ()
                       (dotimes (i num-items)
                         (queue-push q i)))))
         (consumer (make-thread
                     (lambda ()
                       (dotimes (i num-items)
                         (loop
                           (multiple-value-bind (obj success) (queue-pop q)
                             (when success
                               (setf (aref results obj) t)
                               (return)))))))))
    (join-thread producer)
    (join-thread consumer)

    (is (every #'identity results) "All items should be pushed and popped correctly.")))

(defun run-tests ()
  "Run all tests in the suite."
  (run! 'cl-freelock-suite))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-freelock-tests))))
  (run-tests))
