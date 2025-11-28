(defpackage #:cl-freelock-benchmarks
  (:use #:cl #:fl #:bordeaux-threads)
  (:nicknames #:fl-bench)
  (:export #:run-all-benchmarks
           #:run-latency-suite
           #:log-benchmark-result
           #:log-latency-result
           #:print-latency-report
           #:calculate-percentiles
           #:with-gc-pressure-check
           #:*benchmark-log-stream*))

(in-package #:cl-freelock-benchmarks)

;; A helper macro to measure memory allocation
#+sbcl
(defmacro with-gc-pressure-check (&body body)
  `(let ((before (sb-ext:get-bytes-consed)))
     ,@body
     (- (sb-ext:get-bytes-consed) before)))

#-sbcl
(defmacro with-gc-pressure-check (&body body)
  `(progn ,@body 0))


(defvar *benchmark-log-stream* nil)

(defun log-csv-header ()
  "Writes the CSV header, now including Latency columns."
  (when *benchmark-log-stream*
    (format *benchmark-log-stream*
            "Timestamp,Library,OptimizationMode,QueueType,Implementation,ContentionLevel,Producers,Consumers,BatchSize,TotalItems,DurationSec,OpsPerSec,GCBytes,GCEfficiency,ScalingFactor,LatMin,LatP25,LatP50,LatP90,LatP99,LatP999,LatMax~%")
    (finish-output *benchmark-log-stream*)))

(defun log-benchmark-result (queue-type producers consumers batch-size total-items duration-sec ops-per-sec gc-bytes)
  "Logs a throughput benchmark result (Latency columns are empty)."
  (when *benchmark-log-stream*
    (let* ((mode (if (member :cl-freelock-single-threaded *features*) "Single-Threaded" "Multi-Threaded"))
           (library (determine-library queue-type))
           (implementation (determine-implementation queue-type))
           (contention-level (categorize-contention-level producers consumers))
           (gc-efficiency (calculate-gc-efficiency total-items gc-bytes))
           (scaling-factor (calculate-scaling-factor queue-type producers consumers ops-per-sec))
           (timestamp (local-time:format-timestring nil (local-time:now) :format local-time:+rfc3339-format+)))
      
      (format *benchmark-log-stream*
              "~A,~A,~A,~A,~A,~A,~D,~D,~A,~D,~,3F,~,2F,~D,~,2F,~,3F,,,,,,,~%"
              timestamp
              library
              mode
              (categorize-queue-pattern producers consumers)
              implementation  
              contention-level
              producers
              consumers
              (or batch-size "N/A")
              total-items
              duration-sec
              ops-per-sec
              (or gc-bytes 0)
              gc-efficiency
              scaling-factor))
    (finish-output *benchmark-log-stream*)))

(defun log-latency-result (queue-type producers consumers total-items stats)
  "Logs a latency benchmark result (Throughput columns are 0/NA)."
  (when *benchmark-log-stream*
    (let* ((mode (if (member :cl-freelock-single-threaded *features*) "Single-Threaded" "Multi-Threaded"))
           (library "cl-freelock") ;; Latency currently only runs on ours
           (implementation (determine-implementation queue-type))
           (contention-level (categorize-contention-level producers consumers))
           (timestamp (local-time:format-timestring nil (local-time:now) :format local-time:+rfc3339-format+)))
      
      (format *benchmark-log-stream*
              "~A,~A,~A,~A,~A,~A,~D,~D,N/A,~D,0,0,0,0,0,~D,~D,~D,~D,~D,~D,~D~%"
              timestamp
              library
              mode
              (categorize-queue-pattern producers consumers)
              implementation
              contention-level
              producers
              consumers
              total-items
              ;; Latency Columns: p0, p25, p50, p90, p99, p99.9, max
              (getf stats :p0)
              (getf stats :p25)
              (getf stats :p50)
              (getf stats :p90)
              (getf stats :p99)
              (getf stats :p99.9)
              (getf stats :pmax)))
    (finish-output *benchmark-log-stream*)))

(defun log-environment-info ()
  "Log system environment information to the CONSOLE."
  (let ((thread-support (ignore-errors 
                          (and (find-package :bordeaux-threads)
                               (fboundp 'bordeaux-threads:make-thread)
                               "Yes"))))
    (format t "~%# System: ~A ~A, Lisp: ~A ~A, Threads: ~A~%"
            (software-type)
            (software-version) 
            (lisp-implementation-type)
            (lisp-implementation-version)
            (or thread-support "Unknown"))))

(defun categorize-contention-level (producers consumers)
  "Categorize the contention level based on producer/consumer counts."
  (let ((total-threads (+ producers consumers)))
    (cond
      ((and (= producers 1) (= consumers 1)) "1P-1C")
      ((= total-threads 2) "Low")
      ((<= total-threads 8) "Medium") 
      ((<= total-threads 16) "High")
      (t "Very-High"))))

(defun categorize-queue-pattern (producers consumers)
  "Categorize the producer/consumer pattern."
  (cond
    ((and (= producers 1) (= consumers 1)) "SPSC")
    ((= producers 1) "SPMC") 
    ((= consumers 1) "MPSC")
    ((= producers consumers) "Balanced")
    (t "Asymmetric")))

(defun calculate-gc-efficiency (total-items gc-bytes)
  "Calculate GC efficiency as items per KB allocated."
  (if (and gc-bytes (> gc-bytes 0))
      (/ total-items (/ gc-bytes 1024.0))
      (if (zerop total-items) 0 most-positive-fixnum)))

(defun determine-library (queue-type)
  "Determine which library/implementation this queue belongs to."
  (cond
    ((member queue-type '("Unbounded" "Bounded-Single" "Bounded-Batch" "SPSC") :test #'string=) "cl-freelock")
    ((string= queue-type "Lock-Based-List") "Lock-Based")
    ((string= queue-type "oconnore/queues") "oconnore/queues")
    (t "Unknown")))

(defun determine-implementation (queue-type)
  "Determine the specific implementation type."
  (cond
    ((string= queue-type "Unbounded") "Lock-Free-Unbounded")
    ((string= queue-type "Bounded-Single") "Lock-Free-Bounded")
    ((string= queue-type "Bounded-Batch") "Lock-Free-Batch")
    ((string= queue-type "SPSC") "Lock-Free-SPSC")
    ((string= queue-type "Lock-Based-List") "Mutex-Protected-List")
    ((string= queue-type "oconnore/queues") "External-Queue")
    (t queue-type)))

;; Store baseline performance for scaling calculations
(defvar *baseline-performance* (make-hash-table :test 'equal))

(defun store-baseline-performance (queue-type ops-per-sec)
  "Store 1P/1C performance as baseline for scaling calculations."
  (setf (gethash queue-type *baseline-performance*) ops-per-sec))

(defun calculate-scaling-factor (queue-type producers consumers ops-per-sec)
  "Calculate how well performance scales compared to 1P/1C baseline."
  (let ((baseline (gethash queue-type *baseline-performance*)))
    (cond
      ((and (= producers 1) (= consumers 1))
       (store-baseline-performance queue-type ops-per-sec)
       1.0)
      ((and baseline (> baseline 0))
       (let* ((thread-ratio (/ (+ producers consumers) 2.0))
              (ideal-perf (* baseline thread-ratio))
              (scaling-factor (/ ops-per-sec ideal-perf)))
         scaling-factor))
      (t 0.0))))

;;; Latency and Pmax Benchmarks
;;; Discrete Percentiles
;;; NOTE: Consider the fact that there is an initial startup spike, so although pxam is usually quite exact...
;;; it still has the potential to lie to you. Thus, a warmup phase should reduce that bias: ...
;;; Usually you have a function run for about 1 second before the pmax and latency benchmarks...
;;; But, we choose to run them LAST, so that the other benchmarks act as a massive and implicit warmup.
;;; TODO: I eventually want to compare across different architectures and C++
;;; TODO?: Now that I think about it, we should consider adding a tiny explicit warmup for the Unbounded Queue

(defun calculate-percentiles (latencies)
  "Calculates p0, p25, p50, p90, p99, p99.9, and pMax from a list/vector of latencies.
   Latencies are in microseconds (so, E-6)."
  (let* ((sorted (sort (coerce latencies 'vector) #'<))
         (len (length sorted))
         (p0 (aref sorted 0))
         (p25 (aref sorted (floor (* len 0.25))))
         (p50 (aref sorted (floor (* len 0.50))))
         (p90 (aref sorted (floor (* len 0.90))))
         (p99 (aref sorted (floor (* len 0.99))))
         (p99.9 (aref sorted (floor (* len 0.999))))
         (pmax (aref sorted (1- len))))
    (list :p0 p0 :p25 p25 :p50 p50 :p90 p90 :p99 p99 :p99.9 p99.9 :pmax pmax :count len)))

(defun print-latency-report (title percentiles)
  (format t "~&~%--- Latency Report: ~A ---~%" title)
  (format t "    Samples: ~:D~%" (getf percentiles :count))
  (format t "    Min (p0):    ~:D us~%" (getf percentiles :p0))
  (format t "    p25:         ~:D us~%" (getf percentiles :p25))
  (format t "    Median (p50):~:D us~%" (getf percentiles :p50))
  (format t "    p90:         ~:D us~%" (getf percentiles :p90))
  (format t "    p99:         ~:D us~%" (getf percentiles :p99))
  (format t "    p99.9:       ~:D us~%" (getf percentiles :p99.9))
  (format t "    Max:         ~:D us~%" (getf percentiles :pmax)))