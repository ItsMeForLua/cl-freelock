(defsystem #:cl-freelock-benchmarks
  :description "Benchmark suite for cl-freelock."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :depends-on (#:cl-freelock #:bordeaux-threads #:queues #:local-time)
  :serial t
  :components ((:module "benchmarks"
                 :serial t
                 :components
                 ((:file "logging")
                  (:file "unbounded-queue")
                  (:file "bounded-queue")
                  (:file "spsc-queue")
                  (:file "competitors")
                  (:file "latency")
                  (:file "main")))))