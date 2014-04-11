(defpackage :clos-slot-mop
  (:use #:cl #:mop))

(in-package :clos-slot-mop)

(defun setup-tracing ()
  (trace compute-effective-slot-definition-initargs)
  (trace compute-default-initargs)
  (trace compute-effective-slot-definition)
  (trace effective-slot-definition-class)
  (trace compute-slots)
  (trace direct-slot-definition-class)
  (trace make-instance))

(defclass test-class ()
  ((test :accessor test-slot
	 :initarg :test-value
	 :initform 3
	 :documentation "doc"))
  (:default-initargs test 4))
