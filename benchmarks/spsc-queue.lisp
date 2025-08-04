(in-package #:cl-freelock-benchmarks)

(defun run-spsc-benchmark (items)
  "Runs a benchmark for the SPSC queue."
  (let* ((capacity 4096)
         (q (fl:make-spsc-queue capacity))
         (popped-count (fl:make-atomic-ref 0))
         (producer nil)
         (consumer nil))

    (format t "~&~%--- SPSC Queue: 1 Producer, 1 Consumer ---~%")
    (format t "    Total items: ~:D~%" items)

    (let ((start-time (get-internal-real-time)))

      (setf consumer
            (make-thread
             (lambda ()
               (dotimes (i items)
                 ;; Loop until a pop succeeds.
                 (loop
                   (multiple-value-bind (obj success) (fl:spsc-pop q)
                     (when success
                       (atomic-incf popped-count)
                       (return))))))
             :name "SPSC Consumer"))

      (setf producer
            (make-thread
             (lambda ()
               (dotimes (i items)
                 ;; Loop until a push succeeds.
                 (loop until (fl:spsc-push q i))))
             :name "SPSC Producer"))

      ;; Wait for both threads to complete their work.
      (join-thread producer)
      (join-thread consumer)

      ;; Now, we can calculate and print the results.
      (let* ((end-time (get-internal-real-time))
             (duration-seconds (/ (- end-time start-time) internal-time-units-per-second))
             (ops-per-second (if (> duration-seconds 0)
                                 (/ items duration-seconds)
                                 0)))
        (format t "    Duration: ~,3F seconds~%" duration-seconds)
        (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
        ops-per-second))))

(defun run-all-spsc-benchmarks ()
  (format t "~&~%======================================================================~%")
  (format t "--- cl-freelock SPSC Queue Benchmark Suite ---~%")
  (format t "======================================================================~%")
  ;; Use a higher number of items since SPSC should be very fast.
  (run-spsc-benchmark 5000000))
