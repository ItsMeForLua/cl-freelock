;;;; cl-freelock-benchmarks.asd

(defsystem #:cl-freelock-benchmarks
  :description "Benchmark suite for cl-freelock."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :depends-on (#:cl-freelock #:bordeaux-threads)
  :serial t
  :components ((:module "benchmarks"
                 :serial t
                 :components
                 ((:file "main")
                  (:file "bounded-queue")
                  (:file "spsc-queue")))))

