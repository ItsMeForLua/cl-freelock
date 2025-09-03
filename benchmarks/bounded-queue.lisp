(in-package :cl-freelock-benchmarks)

(defun run-bounded-contention-benchmark (num-producers num-consumers items-per-producer)
  "Runs a benchmark for the bounded queue."
  (let* ((capacity 4096)
         (q (fl:make-bounded-queue capacity))
         (total-items (* num-producers items-per-producer))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil)
         (start-time nil)
         (end-time nil)
         (gc-bytes 0))

    (format t "~&~%--- Bounded Queue (Single Item): ~D Producer(s), ~D Consumer(s) ---~%" num-producers num-consumers)
    (format t "    Total items: ~:D~%" total-items)

    (setf gc-bytes
          (with-gc-pressure-check
            (setf start-time (get-internal-real-time))
            (dotimes (i num-consumers)
              (push (make-thread
                     (lambda ()
                       (loop
                         (when (>= (atomic-ref-value popped-count) total-items)
                           (return))
                         (multiple-value-bind (obj success) (bounded-queue-pop q)
                           (when success
                             (atomic-incf popped-count)))
                         (thread-yield)))
                     :name (format nil "Consumer ~D" i))
                    consumers))
            (dotimes (i num-producers)
              (let ((start-item (* i items-per-producer))
                    (end-item (* (1+ i) items-per-producer)))
                (push (make-thread
                       (lambda ()
                         (loop for item from start-item below end-item do
                           (loop until (bounded-queue-push q item) do (thread-yield))))
                       :name (format nil "Producer ~D" i))
                      producers)))
            (mapc #'join-thread producers)
            (loop until (>= (atomic-ref-value popped-count) total-items))
            (mapc #'join-thread consumers)
            (setf end-time (get-internal-real-time))))

    (let* ((duration-sec (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-second (if (plusp duration-sec) (/ total-items duration-sec) 0)))
      (format t "    Duration: ~,3F seconds~%" duration-sec)
      (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
      (format t "    GC Pressure: ~:D bytes~%" gc-bytes)
      (log-benchmark-result "Bounded-Single" num-producers num-consumers
                            nil total-items duration-sec ops-per-second gc-bytes))))

(defun run-bounded-batch-benchmark (num-producers num-consumers items-per-producer &key (batch-size 64))
  "Runs a benchmark for the bounded queue using batch operations."
  (let* ((capacity 8192)
         (q (fl:make-bounded-queue capacity))
         (total-items (* num-producers items-per-producer))
         (actual-total-items (* batch-size (floor total-items batch-size)))
         (num-batches-per-producer (floor (floor actual-total-items num-producers) batch-size))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil)
         (start-time nil)
         (end-time nil)
         (gc-bytes 0))

    (format t "~&~%--- Bounded Queue (Batch Size: ~D): ~D Producer(s), ~D Consumer(s) ---~%" batch-size num-producers num-consumers)
    (format t "    Total items: ~:D (actual produced: ~:D)~%" total-items actual-total-items)

    (setf gc-bytes
          (with-gc-pressure-check
            (setf start-time (get-internal-real-time))
            ;; Create consumers first
            (dotimes (i num-consumers)
              (push (make-thread
                     (lambda ()
                       (loop
                         (when (>= (atomic-ref-value popped-count) actual-total-items)
                           (return))
                         (multiple-value-bind (popped success) (bounded-queue-pop-batch q batch-size)
                           (when success
                             (atomic-incf popped-count (length popped))))
                         (thread-yield)))
                     :name (format nil "Batch Consumer ~D" i))
                    consumers))
            ;; Create producers
            (dotimes (i num-producers)
              (push (make-thread
                     (lambda ()
                       (let ((batch (make-list batch-size :initial-element 0)))
                         (dotimes (j num-batches-per-producer)
                           (loop until (bounded-queue-push-batch q batch)))))
                     :name (format nil "Batch Producer ~D" i))
                    producers))
            ;; Join all producer threads
            (dolist (thread producers)
              (join-thread thread))
            ;; Wait for all items to be consumed
            (loop until (>= (atomic-ref-value popped-count) actual-total-items))
            ;; Join all consumer threads
            (dolist (thread consumers)
              (join-thread thread))
            (setf end-time (get-internal-real-time))))

    (let* ((duration-sec (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-second (if (plusp duration-sec) (/ actual-total-items duration-sec) 0)))
      (format t "    Duration: ~,3F seconds~%" duration-sec)
      (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
      (format t "    GC Pressure: ~:D bytes~%" gc-bytes)
      (log-benchmark-result "Bounded-Batch" num-producers num-consumers
                            batch-size actual-total-items duration-sec ops-per-second gc-bytes))))

(defun run-all-bounded-benchmarks ()
  (format t "~&~%======================================================================~%")
  (format t "--- cl-freelock Bounded Queue Benchmark Suite ---~%")
  (format t "======================================================================~%")

  (let ((num-items 1024000))
    (run-bounded-contention-benchmark 1 1 num-items)
    (run-bounded-contention-benchmark 2 2 (floor num-items 2))
    (run-bounded-contention-benchmark 4 4 (floor num-items 4))
    (run-bounded-contention-benchmark 8 8 (floor num-items 8))
    (run-bounded-contention-benchmark 16 16 (floor num-items 16))
    (run-bounded-contention-benchmark 32 32 (floor num-items 32))
    (run-bounded-contention-benchmark 64 64 (floor num-items 64))

    (run-bounded-batch-benchmark 1 1 num-items)
    (run-bounded-batch-benchmark 2 2 (floor num-items 2))
    (run-bounded-batch-benchmark 4 4 (floor num-items 4))
    (run-bounded-batch-benchmark 8 8 (floor num-items 8))
    (run-bounded-batch-benchmark 16 16 (floor num-items 16))
    (run-bounded-batch-benchmark 32 32 (floor num-items 32))
    (run-bounded-batch-benchmark 64 64 (floor num-items 64))))
