(defpackage #:cl-freelock-benchmarks
  (:use #:cl #:fl #:bordeaux-threads)
  (:nicknames #:fl-bench)
  (:export #:run-all-benchmarks
           #:run-all-bounded-benchmarks
           #:run-all-spsc-benchmarks
           #:run-all-competitor-benchmarks))

(in-package #:cl-freelock-benchmarks)

(defun run-contention-benchmark (num-producers num-consumers items-per-producer)
  "Runs a benchmark for the unbounded queue with a specified number of threads."
  (let* ((q (make-queue))
         (total-items (* num-producers items-per-producer))
         (popped-count (make-atomic-ref 0))
         (producers nil)
         (consumers nil))

    (format t "~&~%--- Unbounded Queue: ~D Producer(s), ~D Consumer(s) ---~%" num-producers num-consumers)
    (format t "    Total items: ~:D~%" total-items)

    (let ((start-time (get-internal-real-time)))

      (dotimes (i num-consumers)
        (push (make-thread
               (lambda ()
                 (loop
                   (multiple-value-bind (obj success) (queue-pop q)
                     (if success
                         (let ((count (atomic-incf popped-count)))
                           (when (>= count total-items)
                             (return)))
                         (progn
                           (when (>= (atomic-ref-value popped-count) total-items)
                             (return))
                           (thread-yield))))))
               :name (format nil "Consumer ~D" i))
              consumers))

      (dotimes (i num-producers)
        (let ((start-item (* i items-per-producer))
              (end-item (* (1+ i) items-per-producer)))
          (push (make-thread
                 (lambda ()
                   (loop for item from start-item below end-item
                         do (queue-push q item)))
                 :name (format nil "Producer ~D" i))
                producers)))

      (mapc #'join-thread producers)
      (mapc #'join-thread consumers)

      (let* ((end-time (get-internal-real-time))
             (duration-seconds (/ (- end-time start-time) internal-time-units-per-second))
             (ops-per-second (if (> duration-seconds 0)
                                 (/ total-items duration-seconds)
                                 0)))
        (format t "    Duration: ~,3F seconds~%" duration-seconds)
        (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
        ops-per-second))))

(defun run-all-unbounded-benchmarks ()
  (format t "~&~%======================================================================~%")
  (format t "--- cl-freelock Unbounded Queue Benchmark Suite ---~%")
  (format t "======================================================================~%")
  
  (let ((num-items 200000))
    (run-contention-benchmark 1 1 num-items)
    (run-contention-benchmark 2 2 (floor num-items 2))
    (run-contention-benchmark 4 4 (floor num-items 4))
    (run-contention-benchmark 4 1 (floor num-items 4))
    (run-contention-benchmark 1 4 num-items)))

(defun run-all-benchmarks ()
  "Run and report on all defined benchmarks."
  (run-all-unbounded-benchmarks)
  (run-all-bounded-benchmarks)
  (run-all-spsc-benchmarks)
  (run-all-competitor-benchmarks)
  (format t "~&~%--- Full Benchmark Suite Complete ---~%"))
