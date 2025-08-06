;;;; benchmarks/unbounded-queue.lisp
;;;; Unbounded queue benchmark tests.

(in-package #:cl-freelock-benchmarks)

(defun run-contention-benchmark (num-producers num-consumers items-per-producer)
  "Runs a benchmark for the unbounded queue."
  (let* ((q (fl:make-queue))
         (total-items (* num-producers items-per-producer))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil)
         (start-time nil)
         (end-time nil)
         (gc-bytes 0))

    (format t "~&~%--- Unbounded Queue: ~D Producer(s), ~D Consumer(s) ---~%" num-producers num-consumers)
    (format t "    Total items: ~:D~%" total-items)

    (setf gc-bytes
          (with-gc-pressure-check
            (setf start-time (get-internal-real-time))
            ;; Create consumers first
            (dotimes (i num-consumers)
              (push (make-thread
                     (lambda ()
                       (loop
                         (when (>= (atomic-ref-value popped-count) total-items)
                           (return))
                         (multiple-value-bind (obj success) (fl:queue-pop q)
                           (when success
                             (atomic-incf popped-count)))
                         (thread-yield)))
                     :name (format nil "Consumer ~D" i))
                    consumers))
            ;; Create producers
            (dotimes (i num-producers)
              (let ((start-item (* i items-per-producer))
                    (end-item (* (1+ i) items-per-producer)))
                (push (make-thread
                       (lambda ()
                         (loop for item from start-item below end-item do
                           (fl:queue-push q item)))
                       :name (format nil "Producer ~D" i))
                      producers)))
            ;; Join all producer threads
            (dolist (thread producers)
              (join-thread thread))
            ;; Wait for all items to be consumed
            (loop until (>= (atomic-ref-value popped-count) total-items))
            ;; Join all consumer threads
            (dolist (thread consumers)
              (join-thread thread))
            (setf end-time (get-internal-real-time))))

    (let* ((duration-sec (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-second (if (plusp duration-sec) (/ total-items duration-sec) 0)))
      (format t "    Duration: ~,3F seconds~%" duration-sec)
      (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
      (format t "    GC Pressure: ~:D bytes~%" gc-bytes)
      (log-benchmark-result "Unbounded" num-producers num-consumers
                            nil total-items duration-sec ops-per-second gc-bytes))))

(defun run-all-unbounded-benchmarks ()
  (format t "~&~%======================================================================~%")
  (format t "--- cl-freelock Unbounded Queue Benchmark Suite ---~%")
  (format t "======================================================================~%")
  (let ((num-items 200000))
    (run-contention-benchmark 1 1 num-items)
    (run-contention-benchmark 2 2 (floor num-items 2))
    (run-contention-benchmark 4 4 (floor num-items 4))
    (run-contention-benchmark 8 8 (floor num-items 8))
    (run-contention-benchmark 16 16 (floor num-items 16))
    (run-contention-benchmark 32 32 (floor num-items 32))
    (run-contention-benchmark 64 64 (floor num-items 64))
    (run-contention-benchmark 4 1 (floor num-items 4))
    (run-contention-benchmark 1 4 num-items)))
