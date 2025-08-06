(in-package #:cl-freelock-benchmarks)

(defun run-all-benchmarks (&key (log-file nil) (append nil))
  "Run and report on all defined benchmarks.
   - LOG-FILE: Path to the CSV file.
   - APPEND: If T, appends to the log file. Otherwise, creates a new one."
  (log-environment-info)

  (flet ((run-suites ()
           (run-all-unbounded-benchmarks)
           (run-all-bounded-benchmarks)
           (run-all-spsc-benchmarks)
           (run-all-competitor-benchmarks)))
    
    (if log-file
        (with-open-file (*benchmark-log-stream* log-file
                                                :direction :output
                                                :if-exists (if append :append :supersede)
                                                :if-does-not-exist :create)
          (format t "~&--> Logging benchmark results to ~A~%" (truename *benchmark-log-stream*))
          ;; Only write the header if we are creating a new file
          (unless append
            (log-csv-header))
          (run-suites))
        (run-suites)))
  
  (format t "~&~%Benchmark run complete.~%"))
