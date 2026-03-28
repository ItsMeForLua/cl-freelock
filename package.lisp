;; package.lisp

;; I haven't decided yet if I want this file to be in src/ or root.

;; The #: before symbols in defpackage is considered a good practice...
;; to make the symbols read as uninterned and then...
;; interned into the new package by defpackage.
;; The #: notation helps avoid accidental...
;; namespace clutter and/or collisions during compilation or editing.

(defpackage :cl-freelock
  (:use #:cl)
  (:nicknames #:fl)
  (:export
   #:atomic-ref
   #:make-atomic-ref
   #:atomic-ref-value
   #:cas
   #:atomic-incf
   #:memory-barrier

   ;; Lock-free Unbounded Queue
   #:queue
   #:make-queue
   #:queue-push
   #:queue-pop
   #:queue-empty-p
   
   ;; Lock-free Bounded Queue
   #:bounded-queue
   #:make-bounded-queue
   #:bounded-queue-push
   #:bounded-queue-pop
   #:bounded-queue-full-p
   #:bounded-queue-empty-p
   #:bounded-queue-capacity
   #:bounded-queue-push-batch
   #:bounded-queue-pop-batch

   ;; Lock-free SPSC Queue
   #:spsc-queue
   #:make-spsc-queue
   #:spsc-push
   #:spsc-pop))
