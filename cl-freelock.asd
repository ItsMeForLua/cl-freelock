(defsystem #:cl-freelock
  :description "lock-free concurrency primitives, written in pure Common Lisp."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "atomics")
               (:file "queue")
               (:file "bounded-queue")
               (:file "spsc-queue")))
