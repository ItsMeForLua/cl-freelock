(asdf:defsystem #:cl-freelock-tests
  :description "Test suite for cl-freelock."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :depends-on (:cl-freelock #:fiveam #:bordeaux-threads)
  :components ((:module "tests"
                 :serial t
                 :components
                 ((:file "main")
                  (:file "bounded-queue")
                  (:file "spsc-queue"))))
  :in-order-to ((test-op (load-op cl-freelock-tests))))

;; I haven't decided yet on if I want to use `:module` or `"subdir/file"`in our asd files.