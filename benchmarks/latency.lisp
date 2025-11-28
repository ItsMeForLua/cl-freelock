;;;; benchmarks/latency.lisp

(in-package :cl-freelock-benchmarks)

;;; some timing helpers
;;; We use microseconds (E-6).
;;; NOTE: Standard internal-real-time is usually only millisecond precision.

#+sbcl
(declaim (inline current-time-micros))
#+sbcl
(defun current-time-micros ()
  "Returns current time in microseconds."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* sec 1000000) usec)))

#-sbcl
(defun current-time-micros ()
  "Fallback for non-SBCL (might be low resolution)."
  (* (get-internal-real-time)
     (floor 1000000 internal-time-units-per-second)))

;;; Benchmark Runner

(defun run-generic-latency-benchmark (name queue-type make-queue-fn push-fn pop-fn num-producers num-consumers items-per-producer)
  "Generic driver for latency benchmarks.
   - Pushed items are timestamps (fixnums).
   - Consumers calculate (current-time - pushed-time)."
  
  (let* ((q (funcall make-queue-fn))
         (total-items (* num-producers items-per-producer))
         (popped-count (fl:make-atomic-ref 0))
         (producers nil)
         (consumers nil)
         ;; We pre allocate a results array to avoid GC noise during measurement.
         ;; We make it thread-safe by giving each consumer a "slice" OR using a concurrent collect.
         (consumer-results (make-array num-consumers :initial-element nil)))

    (format t "~&~%Run Latency Bench: ~A (~DP/~DC) Total: ~:D~%" name num-producers num-consumers total-items)

    ;; (1) Setup Consumers
    (dotimes (i num-consumers)
      ;; Rebind 'i' to ensure the thread closure captures the VALUE, not the loop variable...
      ;; This is called variable shadowing, and we use it to prevent racing conditions(in this case, "loop variable capture")
      (let ((i i)) 
        (let* ((t-name (format nil "Latency-Consumer-~D" i))
               (th (make-thread
                    (lambda ()
                      (let ((my-latencies (make-array (ceiling (* total-items 1.2) num-consumers) 
                                                      :fill-pointer 0 :element-type 'fixnum))
                            (c-idx i)) 
                        (loop
                          (when (>= (atomic-ref-value popped-count) total-items)
                            (setf (aref consumer-results c-idx) my-latencies)
                            (return))
                          
                          (multiple-value-bind (ts success) (funcall pop-fn q)
                            (when success
                              (let ((now (current-time-micros)))
                                (when (integerp ts)
                                  (vector-push-extend (- now ts) my-latencies))
                                (atomic-incf popped-count))))
                          
                          (thread-yield))))
                    :name t-name)))
          (push th consumers))))

    ;; (2) Setup Producers
    (dotimes (i num-producers)
      (let ((i i)) ;; Rebind i here as well for safety even though not technically needed.
        (let* ((t-name (format nil "Latency-Producer-~D" i))
               (th (make-thread
                    (lambda ()
                      (dotimes (j items-per-producer)
                        (let ((ts (current-time-micros)))
                          (loop until (funcall push-fn q ts)
                                do (thread-yield)))))
                    :name t-name)))
          (push th producers))))

    ;; (3)
    (mapc #'join-thread producers)
    (loop until (>= (atomic-ref-value popped-count) total-items)
          do (sleep 0.01))
    (mapc #'join-thread consumers)

    ;; (4)
    (let ((all-latencies (make-array total-items :fill-pointer 0 :adjustable t :element-type 'fixnum)))
      (loop for res across consumer-results
            when res do
            (loop for lat across res do
                  (vector-push-extend lat all-latencies)))
      
      (if (zerop (length all-latencies))
          (format t "No latency data collected!~%")
          (let ((stats (calculate-percentiles all-latencies)))
            (print-latency-report (format nil "~A (~DP/~DC)" name num-producers num-consumers) stats)
            (log-latency-result queue-type num-producers num-consumers total-items stats))))))

(defun bench-unbounded-latency (producers consumers items)
  (run-generic-latency-benchmark 
   "Unbounded Queue"
   "Unbounded"
   #'fl:make-queue 
   #'fl:queue-push 
   #'fl:queue-pop 
   producers consumers items))

(defun bench-bounded-latency (producers consumers items)
  (run-generic-latency-benchmark 
   "Bounded Queue"
   "Bounded-Single"
   (lambda () (fl:make-bounded-queue 4096)) ; Our default capacity for benchmarks
   #'fl:bounded-queue-push 
   #'fl:bounded-queue-pop 
   producers consumers items))

(defun bench-spsc-latency (items)
  (run-generic-latency-benchmark 
   "SPSC Queue"
   "SPSC"
   (lambda () (fl:make-spsc-queue 4096))
   #'fl:spsc-push 
   #'fl:spsc-pop 
   1 1 items))

(defun run-latency-suite ()
  "Runs the full suite requested by the user."
  (format t "~&~%======================================================================~%")
  (format t "--- Latency Benchmark Suite (Microseconds) ---~%")
  (format t "======================================================================~%")
  
  (let ((items 200000))
    ;; SPSC
    (bench-spsc-latency items)

    ;; Bounded Queue (SPMC/MPSC/MPMC)
    (format t "~&~%-- Bounded Queue Latency --~%")
    (bench-bounded-latency 1 1 items)      ; Baseline
    (bench-bounded-latency 1 4 items)      ; SPMC
    (bench-bounded-latency 4 1 items)      ; MPSC
    (bench-bounded-latency 4 4 items)      ; Balanced

    ;; Unbounded Queue
    (format t "~&~%-- Unbounded Queue Latency --~%")
    (bench-unbounded-latency 1 1 items)
    (bench-unbounded-latency 1 4 items)
    (bench-unbounded-latency 4 4 items)))