(in-package #:cl-freelock-benchmarks)

(defun run-all-benchmarks (&key (log-file nil) (append nil) (include-latency t))
  "Run and report on all defined benchmarks.
   - LOG-FILE: Path to the CSV file.
   - APPEND: If T, appends to the log file. Otherwise, creates a new one.
   - INCLUDE-LATENCY: If T, runs the latency suite as well."
  (log-environment-info)

  ;; Define the execution sequence locally so we can reuse it
  (flet ((execute-suites ()
           (run-all-unbounded-benchmarks)
           (run-all-bounded-benchmarks)
           (run-all-spsc-benchmarks)
           (run-all-competitor-benchmarks)
           (when include-latency
             (run-latency-suite))))
    
    (if log-file
        (with-open-file (*benchmark-log-stream* log-file
                                                :direction :output
                                                :if-exists (if append :append :supersede)
                                                :if-does-not-exist :create)
          (format t "~&--> Logging benchmark results to ~A~%" (truename *benchmark-log-stream*))
          
          ;; Only write the header if we are creating a new file
          (unless append
            (log-csv-header))
          
          (execute-suites))
        
        (execute-suites)))
  
  (format t "~&~%Benchmark run complete.~%"))