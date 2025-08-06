(in-package #:cl-freelock-benchmarks)

;; Lock-based queue implementation for comparison
(defclass lock-based-queue ()
  ((items :initform nil :accessor lbq-items)
   (lock :initform (make-lock "LBQ-Lock") :reader lbq-lock)))

(defun lbq-push (queue item)
  "Push an item to the lock-based queue."
  (with-lock-held ((lbq-lock queue))
    (push item (lbq-items queue)))
  t)

(defun lbq-pop (queue)
  "Pop an item from the lock-based queue. Returns the item or NIL if empty."
  (with-lock-held ((lbq-lock queue))
    (when (lbq-items queue)
      (pop (lbq-items queue)))))

(defun run-single-threaded-baseline (total-items)
  "Runs a single-threaded baseline test to establish theoretical maximum throughput.
This is not a queue implementation but rather a baseline for comparison."
  (let ((start-time nil)
        (end-time nil)
        (gc-bytes 0)
        (counter 0))

    (format t "~&~%--- Single-Threaded Baseline (Theoretical Maximum) ---~%")
    (format t "    Total items: ~:D~%" total-items)

    (setf gc-bytes
          (with-gc-pressure-check
            (setf start-time (get-internal-real-time))
            ;; Simple arithmetic operations to simulate work
            (dotimes (i total-items)
              (setf counter (+ counter i)))
            (setf end-time (get-internal-real-time))))

    (let* ((duration-sec (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-second (if (plusp duration-sec) (/ total-items duration-sec) 0)))
      (format t "    Duration: ~,3F seconds~%" duration-sec)
      (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
      (format t "    GC Pressure: ~:D bytes~%" gc-bytes)
      (format t "    Counter result: ~:D~%" counter)
      ;; We don't log the baseline as it's not a queue implementation
      (format t "    (Baseline - not logged as it's theoretical maximum, not a queue)~%"))))

(defun run-lock-based-benchmark (num-producers num-consumers total-items)
  "Runs a benchmark for a simple list protected by a bordeaux-threads lock."
  (let* ((q (make-instance 'lock-based-queue))
         (items-per-producer (floor total-items num-producers))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil)
         (start-time nil)
         (end-time nil)
         (gc-bytes 0))
    (format t "~&~%--- Competitor: Lock-based list: ~D Producer(s), ~D Consumer(s) ---~%" num-producers num-consumers)
    (format t "    Total items: ~:D~%" total-items)

    (setf gc-bytes
          (with-gc-pressure-check
            (setf start-time (get-internal-real-time))
            (dotimes (i num-consumers)
              (push (make-thread (lambda () (loop (when (>= (atomic-ref-value popped-count) total-items) (return))
                                                   (when (lbq-pop q) (atomic-incf popped-count)))))
                    consumers))
            (dotimes (i num-producers)
              (push (make-thread (lambda () (dotimes (j items-per-producer) (lbq-push q j))))
                    producers))
            (dolist (thread producers)
              (join-thread thread))
            (loop until (>= (atomic-ref-value popped-count) total-items))
            (dolist (thread consumers)
              (join-thread thread))
            (setf end-time (get-internal-real-time))))

    (let* ((duration-sec (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-second (if (plusp duration-sec) (/ total-items duration-sec) 0)))
      (format t "    Duration: ~,3F seconds~%" duration-sec)
      (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
      (format t "    GC Pressure: ~:D bytes~%" gc-bytes)
      (log-benchmark-result "Lock-Based-List" num-producers num-consumers
                            nil total-items duration-sec ops-per-second gc-bytes))))

(defun run-oconnore-queue-benchmark (num-producers num-consumers total-items)
  "Runs a benchmark for oconnore/queues' safe-queue."
  (ql:quickload :queues.simple-cqueue :silent t)
  (let* ((q (queues:make-queue :simple-cqueue))
         (items-per-producer (floor total-items num-producers))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil)
         (start-time nil)
         (end-time nil)
         (gc-bytes 0))
    (format t "~&~%--- Competitor: oconnore/queues: ~D Producer(s), ~D Consumer(s) ---~%" num-producers num-consumers)
    (format t "    Total items: ~:D~%" total-items)

    (setf gc-bytes
          (with-gc-pressure-check
            (setf start-time (get-internal-real-time))
            (dotimes (i num-consumers)
              (push (make-thread (lambda () (loop (when (>= (atomic-ref-value popped-count) total-items) (return))
                                                   (multiple-value-bind (obj success) (queues:qpop q)
                                                     (when success (atomic-incf popped-count))))))
                    consumers))
            (dotimes (i num-producers)
              (push (make-thread (lambda () (dotimes (j items-per-producer) (queues:qpush q j))))
                    producers))
            (dolist (thread producers)
              (join-thread thread))
            (loop until (>= (atomic-ref-value popped-count) total-items))
            (dolist (thread consumers)
              (join-thread thread))
            (setf end-time (get-internal-real-time))))

    (let* ((duration-sec (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-second (if (plusp duration-sec) (/ total-items duration-sec) 0)))
      (format t "    Duration: ~,3F seconds~%" duration-sec)
      (format t "    Throughput: ~,2F operations/sec~%" ops-per-second)
      (format t "    GC Pressure: ~:D bytes~%" gc-bytes)
      (log-benchmark-result "oconnore/queues" num-producers num-consumers
                            nil total-items duration-sec ops-per-second gc-bytes))))

(defun run-all-competitor-benchmarks ()
  (format t "~&~%======================================================================~%")
  (format t "--- Competitor Benchmark Suite ---~%")
  (format t "======================================================================~%")
  (let ((num-items 1000000))
    ;; Baseline isn't logged as it's a theoretical maximum, not a queue implementation.
    (run-single-threaded-baseline (* 2 num-items))

    (format t "~&~%-- Low Contention (1P/1C) --~%")
    (run-lock-based-benchmark 1 1 num-items)
    (run-oconnore-queue-benchmark 1 1 num-items)

    (format t "~&~%-- Medium Contention (4P/4C) --~%")
    (run-lock-based-benchmark 4 4 num-items)
    (run-oconnore-queue-benchmark 4 4 num-items)

    (format t "~&~%-- High Contention (8P/8C) --~%")
    (run-lock-based-benchmark 8 8 num-items)
    (run-oconnore-queue-benchmark 8 8 num-items)

    (format t "~&~%-- Very High Contention (16P/16C) --~%")
    (run-lock-based-benchmark 16 16 num-items)
    (run-oconnore-queue-benchmark 16 16 num-items)

    (format t "~&~%-- Extremely High Contention (32P/32C) --~%")
    (run-lock-based-benchmark 32 32 num-items)
    (run-oconnore-queue-benchmark 32 32 num-items)

    (format t "~&~%-- Massive Contention (64P/64C) --~%")
    (run-lock-based-benchmark 64 64 num-items)
    (run-oconnore-queue-benchmark 64 64 num-items)

    (format t "~&~%-- MPSC Contention (4P/1C) --~%")
    (run-lock-based-benchmark 4 1 num-items)
    (run-oconnore-queue-benchmark 4 1 num-items)

    (format t "~&~%-- SPMC Contention (1P/4C) --~%")
    (run-lock-based-benchmark 1 4 num-items)
    (run-oconnore-queue-benchmark 1 4 num-items)))
