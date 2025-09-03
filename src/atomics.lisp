(in-package :cl-freelock)

;; Wrapper around the atomic operations, primarily Compare-and-Swap (CAS).

;; There are some underlying assumptions here that need to be tested against.
;; We need to do some more thorough testing against other implementations.
#-(or sbcl ccl ecl)
(error "cl-freelock requires atomic operations support from the Lisp implementation. Supported: SBCL, CCL, ECL.")

;; We define a simple structure to hold a value that can be atomically updated.
;; This acts as our atomic pointer/reference.
(defstruct (atomic-ref (:constructor make-atomic-ref (value)))
  (value (error "Value is required") :read-only nil))

(defmacro cas (place old new)
  "A portable Compare-and-Swap macro.
   Atomically stores NEW in PLACE if the current value of PLACE is EQ to OLD.
   Returns the old value of PLACE. The comparison is done by EQ."
  #+sbcl
  `(sb-ext:compare-and-swap (atomic-ref-value ,place) ,old ,new)
  #+ccl
  `(ccl::conditional-store (atomic-ref-value ,place) ,old ,new)
  #+ecl
  `(mp:compare-and-swap (atomic-ref-value ,place) ,old ,new)
  #-(or sbcl ccl ecl)
  `(error "CAS not implemented for this Lisp."))

(defmacro atomic-incf (atomic-ref &optional (delta 1))
  "Atomically increments the value of the atomic-ref by DELTA.
   Returns the new value. Implemented via a CAS loop for portability."
  `(loop
     (let* ((old-val (atomic-ref-value ,atomic-ref))
            (new-val (+ old-val ,delta)))
       (when (eq old-val (cas ,atomic-ref old-val new-val))
         (return new-val)))))

(declaim (inline thread-yield))
(defun thread-yield ()
  "A portable function to yield the current thread's timeslice to the OS."
  #+sbcl
  (sb-thread:thread-yield)
  #+ccl
  (ccl:process-allow-schedule)
  #+ecl
  (mp:process-yield)
  #-(or sbcl ccl ecl)
  ;; A reasonable fallback (as far as I'm aware) for other implementations is a tiny sleep.
  (sleep 0.000001))

;; We need a dummy atomic ref for the SBCL barrier workaround.
#+sbcl
(defvar *sbcl-barrier-dummy* (make-atomic-ref nil))

(declaim (inline memory-barrier))
(defun memory-barrier ()
  "Full memory barrier. All memory operations before 
  the barrier are completed before any memory operations after it."
  #+sbcl
  ;; WORKAROUND for buggy/inconsistent sb-thread:barrier in some SBCL
  ;; versions. A compare-and-swap operation acts as a full memory barrier,
  ;; so we perform a dummy CAS on a global variable. This is less efficient
  ;; than a direct barrier, but is guaranteed to work. This will be used until I find a better option.
  (cas *sbcl-barrier-dummy* nil nil)
  #+ccl
  (ccl:hard-memory-barrier)
  #+ecl
  (progn)
  #-(or sbcl ccl ecl)
  (error "Memory barrier not implemented for this Lisp."))
