(in-package :cl-freelock-benchmarks)

(defun run-spsc-benchmark (items)
  "Runs a benchmark for the SPSC queue."
  (let* ((capacity 4096)
         (q (fl:make-spsc-queue capacity))
         (popped-count (fl:make-atomic-ref 0))
         (producer nil)
         (consumer nil)
         (start-time nil)
         (end-time nil)
         (gc-bytes 0))

    (format t "~&~%--- SPSC Queue: 1 Producer, 1 Consumer ---~%")
    (format t "    Total items: ~:D~%" items)

    (setf gc-bytes
          (with-gc-pressure-check
            (setf start-time (get-internal-real-time))
            (setf consumer
                  (make-thread
                   (lambda ()
                     (dotimes (i items)
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
                       (loop until (fl:spsc-push q i))))
                   :name "SPSC Producer"))
            (join-thread producer)
            (join-thread consumer)
            (setf end-time (get-internal-real-time))))

    (let* ((duration-sec (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-second (if (plusp duration-sec) (/ items duration-sec) 0)))
      (format t "    Duration: ~,3F seconds~%" duration-sec)
      (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
      (format t "    GC Pressure: ~:D bytes~%" gc-bytes)
      (log-benchmark-result "SPSC" 1 1 nil items duration-sec ops-per-second gc-bytes))))

(defun run-all-spsc-benchmarks ()
  (format t "~&~%======================================================================~%")
  (format t "--- cl-freelock SPSC Queue Benchmark Suite ---~%")
  (format t "======================================================================~%")
  (run-spsc-benchmark 5000000))
