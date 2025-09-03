;; Using #: here would create uninterned symbols that may not match up 
;; properly when referenced by other ASDF commands or dependencies. 
;; Keywords (:) provide the intended package-independent,
;; canonical name for systems, dependencies, and options in ASDF.

;; Specifically add `asdf:` to defsystem's because some older implementations
;; use their own "defsystem" which would cause conflicts.
(asdf:defsystem :cl-freelock
  :description "lock-free concurrency primitives, written in pure Common Lisp."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "0.1.1"
  :serial t
  :components ((:file "package")
               (:file "src/atomics")
               (:file "src/queue")
               (:file "src/bounded-queue")
               (:file "src/spsc-queue")))

;; TODO: 
;; We need to make the docstrings a bit more consistent and user-friendly...
;; There are instances of docstrings in functions that don't need them...
;; Looking at other libraries, I noticed some libraries have very thorough docstrings.
;; Though that may be overkill here since the usage is fairly straightforward.

;; I would like to see if cl-freelock is compatible with cando/clasp