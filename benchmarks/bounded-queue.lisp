(in-package #:cl-freelock-benchmarks)

(defun run-bounded-contention-benchmark (num-producers num-consumers items-per-producer)
  "Runs a benchmark for the bounded queue."
  (let* ((capacity 4096)
         (q (fl:make-bounded-queue capacity))
         (total-items (* num-producers items-per-producer))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil))

    (format t "~&~%--- Bounded Queue (Single Item): ~D Producer(s), ~D Consumer(s) ---~%" num-producers num-consumers)
    (format t "    Total items: ~:D~%" total-items)

    (let ((start-time (get-internal-real-time)))

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
                   (loop for item from start-item below end-item
                         do (loop until (bounded-queue-push q item)
                                  do (thread-yield))))
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

(defun run-bounded-batch-benchmark (num-producers num-consumers total-items batch-size)
  "Runs a benchmark for the bounded queue using the batch API."
  (let* ((capacity 8192)
         (q (fl:make-bounded-queue capacity))
         (items-per-producer (floor total-items num-producers))
         ;; We make sure it calculates the actual number of items that will be produced to prevent racing.
         (num-batches-per-producer (floor items-per-producer batch-size))
         (actual-total-items (* num-producers num-batches-per-producer batch-size))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil))

    (format t "~&~%--- Bounded Queue (Batch Size: ~D): ~D Producer(s), ~D Consumer(s) ---~%" batch-size num-producers num-consumers)
    (format t "    Total items: ~:D (actual produced: ~:D)~%" total-items actual-total-items)

    (let ((start-time (get-internal-real-time)))
      (dotimes (i num-consumers)
        (push (make-thread
               (lambda ()
                 (loop
                   ;; We make sure to use actual-total-items for the exit condition
                   (when (>= (atomic-ref-value popped-count) actual-total-items) (return))
                   (multiple-value-bind (batch success) (bounded-queue-pop-batch q batch-size)
                     (when success
                       (atomic-incf popped-count (length batch))))
                   (thread-yield)))
               :name (format nil "Batch Consumer ~D" i))
              consumers))

      (dotimes (i num-producers)
        (push (make-thread
               (lambda ()
                 (let ((batch (make-array batch-size :initial-element 0)))
                   ;; Use the pre-calculated number of batches
                   (dotimes (j num-batches-per-producer)
                     (loop until (bounded-queue-push-batch q batch)))))
               :name (format nil "Batch Producer ~D" i))
              producers))

      (mapc #'join-thread producers)
      (loop until (>= (atomic-ref-value popped-count) actual-total-items))
      (mapc #'join-thread consumers)

      (let* ((end-time (get-internal-real-time))
             (duration-seconds (/ (- end-time start-time) internal-time-units-per-second))
             (ops-per-second (if (> duration-seconds 0)
                                 (/ actual-total-items duration-seconds)
                                 0)))
        (format t "    Duration: ~,3F seconds~%" duration-seconds)
        (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
        ops-per-second))))

(defun run-all-bounded-benchmarks ()
  (format t "~&~%======================================================================~%")
  (format t "--- cl-freelock Bounded Queue Benchmark Suite ---~%")
  (format t "======================================================================~%")

  (let ((num-items 1024000)) ; We make sure to use a number cleanly divisible by 4 and 64
    (run-bounded-contention-benchmark 1 1 num-items)
    (run-bounded-contention-benchmark 2 2 (floor num-items 2))
    (run-bounded-contention-benchmark 4 4 (floor num-items 4))
    
    (run-bounded-batch-benchmark 1 1 num-items 64)
    (run-bounded-batch-benchmark 2 2 num-items 64)
    (run-bounded-batch-benchmark 4 4 num-items 64)))
