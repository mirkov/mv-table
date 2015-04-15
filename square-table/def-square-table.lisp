(in-package :table-dev)

;; this file deals only with the def-square-table macro.  It is a long
;; macro (according to CCL), and I want it cleanly documented.


(defun canonicalize-option-as-slot (option)
  "Process deftable options and put them into slot form.  Return NIL for unrecognized
deftable options.

 (:option &rest values)
is transformed into 
 (option :reader :option
	 :initform values
	 :allocation :class)

Signal error for unrecognized or illegal options.

We check that options have values
We do not check that there are no duplicate options - we leave that to closs
We check that :metaclass has not been specified
We check that values does not have :allocation 


This is a top-level function that calls a few helper functions to do
its job"
  (destructuring-bind (option-name &rest option-values) option
    (assert option-values ()
	    (error 'table-option-missing-value :option-name option-name))
    #+we-check-for-duplicates(check-duplicate-option option-name)
    (if (member option-name *def-square-table-options*)
	(let ((local-symbol (intern (symbol-name option-name) :table-dev)))
	  `(,local-symbol :reader ,local-symbol
			  :initform ,@option-values
			  :allocation :class))
	nil)))
		 

#+clos-checks-for-duplicates(defun check-duplicate-option (option-name)
  "Check for duplicate options in options-seen"
  (declare (special options-seen))
  (if (member option-name options-seen :test #'eq)
      (error 'duplicate-table-option :option-name option-name) 
      (push option-name options-seen)))
(defparameter *def-square-table-options*
  '(:value-type :value-normalizer :default-value
    :equality-predicate :comparator
    :adjustable)
  "Legal square table options")


(define-test canonicalize-option-as-slot
  "Spot check of the functions main operation"
  (assert-equal '(value-type :reader value-type
			    :initform 'integer
			    :allocation :class)
		(canonicalize-option-as-slot '(:value-type 'integer)))
  (assert-true
		(canonicalize-option-as-slot '(:comparator
					       (lambda (x y)
						 t))))
  (assert-error 'TABLE-OPTION-MISSING-VALUE
		(canonicalize-option-as-slot '(:value-type)))
  (assert-true (not (canonicalize-option-as-slot '(:documentation "foo")))))


(defun canonicalize-option-as-defclass-option (option)
  "Process class options:
- :metaclass is not allowed and we signal an error
- :default-initargs is not handled, we signal an error.  This needs to
  be fixed in a sensible manner by incorpoating ccl code
- :documentation is handled"
  (destructuring-bind (option-name &rest option-values) option
  (case option-name
    (:metaclass (error 'illegal-table-option :option-name option-name))
    #+skip(:default-initargs `(:default-initargs ,@option-values))
    (t `(,option-name ,@option-values)))))

(define-test canonicalize-option-as-defclass-option
  (assert-error 'illegal-table-option
		(canonicalize-option-as-defclass-option '(:metaclass 'foo)))
  (assert-equal '(:documentation "text")
		(canonicalize-option-as-defclass-option '(:documentation "text")))
  (assert-equal '(:default-initargs
		  :slot0 val0
		  :slot1 val1)
		(canonicalize-option-as-defclass-option '(:default-initargs
							  :slot0 val0 :slot1 val1))))





(defun canonicalize-table-super (table-super)
  "We only ensure that the table-super is a symbol"
  (assert (and table-super
	       (symbolp table-super)) ()
	       (error 'illegal-table-name :table-name table-super))
  table-super)

(define-test canonicalize-table-super
  (assert-true (canonicalize-table-super 'foo))
  (assert-error 'illegal-table-name (canonicalize-table-super 5)))


(defun canonicalize-table-slot (slot)
  "check that ROW-LABELS-SCHEMA and COL-LABELS-SCHEMA have a :NAMES
option.  Everything else is passed through"
#+we-check-for-duplicates(declare (special slot-options-seen))
#+we-check-for-duplicates(check-duplicate-slot slot-name)
#+we-check-for-duplicates(setf slot-options-seen nil)
  (destructuring-bind (slot-name &rest slot-options) slot
    (assert slot-name ()
	    (error 'illegal-table-slot-name :slot-name slot-name))
    (when (or (eql slot-name 'row-labels-schema)
	      (eql slot-name 'col-labels-schema))
      (assert (find :names slot-options) ()
	      (error 'missing-table-slot-option :slot-option-name :names
		     :slot-name slot-name)))
    `(,slot-name ,@slot-options)))


(define-test canonicalize-table-slot
  (declare (special slots-seen))
  (assert-equal '(row-labels-schema :names (a b c) :default-value 3.14)
		(canonicalize-table-slot '(row-labels-schema :names (a b c)
					   :default-value 3.14)))
  (assert-error 'illegal-table-slot-name
		(canonicalize-table-slot '(nil :names (a b c)
					   :default-value 3.14)))
  (assert-error 'missing-table-slot-option
		(canonicalize-table-slot '(row-labels-schema 
					   :default-value 3.14
					   :default-value 2.71))))

;; we leave to CLOS to handle duplicate slots
#+we-check-for-duplicates(defun check-duplicate-slot (slot-name)
  "Check for duplicate slots in slots-seen"
  (declare (special slots-seen))
  (if (member slot-name slots-seen :test #'eq)
      (error 'duplicate-table-slot :slot-name slot-name) 
      (push slot-name slots-seen)))

;; we leave to CLOS to handle duplicate slot-options
#+we-check-for-duplicates(defun check-duplicate-slot-option (option-name slot-name)
  "Check for duplicate slots in slots-seen"
  (declare (special slot-options-seen))
  (if (member option-name slot-options-seen :test #'eq)
      (error 'duplicate-table-slot-option :slot-name slot-name
	     :slot-option-name option-name) 
      (push option-name slot-options-seen)))

#+obsolete(defun canonicalize-table-slot-by-name (name options)
  "Canonicalize table slot"
  (unless (or (eql name 'row-labels-schema)
	      (eql name 'col-labels-schema))
    (error 'illegal-table-slot-name :slot-name name))
  (let (names
	value-type
	value-normalizer
	equality-predicate
	comparator
	default-value
	adjustable)
    (do ((options options (cddr options)))
	((null options))
      (destructuring-bind (option-name option-value &rest rest-of-options) options
	(declare (ignore rest-of-options))
	#+not-implementted(unless option-value (error 'missing-table-slot-option-value
						      :slot-name name
						      :slot-option-name option-name))
	(macrolet ((assert-function ()
		     `(assert (functionp option-value) ()
			      (error 'illegal-table-slot-option-value
				     :slot-name name
				     :slot-option-name option-name
				     :slot-option-value option-value))))
	  (print option-name)
	  (print option-value)
	  (print (type-of option-value))
	  (case option-name
	    (:names (setf names option-value))
	    (:value-type (setf value-type option-value))
	    (:value-normalizer #+skip(assert-function) (setf value-normalizer option-value))
	    (:equality-predicate #+skip(assert-function) (setf equality-predicate
							 option-value))
	    (:comparator #+skip(assert-function) (setf comparator option-value))
	    (:default-value (setf default-value option-value))
	    (:adjustable (setf adjustable option-value))
	    (t (error 'illegal-table-slot-option))))))
    `(,name
      :names ',names
      ,@(when value-type `(:value-type ,value-type))
      ,@(when value-normalizer `(:value-normalizer ,value-normalizer))
      ,@(when equality-predicate `(:equality-predicate ,equality-predicate))
      ,@(when comparator `(:comparator ,comparator))
      ,@(when default-value `(:default-value ,default-value))
      ,@(when adjustable `(:adjustable ,adjustable)))))




(defmacro def-square-table (table-name table-supers slots
			    &rest table-options
			    &environment env)
  "prepare to call ensure-class-using-class

Table options are converted to slots, and both them and the slots are
checked for corectness"
  (declare (ignore env))
  (declare (special options-seen slots-seen))
  (assert (and table-name
	       (symbolp table-name)) ()
	       (error 'invalid-table-name :name table-name))
  (setf options-seen nil
	slots-seen nil)
    (let ((canonicalized-supers (mapcar #'canonicalize-table-super table-supers))
	   (canonicalized-slots (mapcar #'canonicalize-table-slot slots))
	   (canonicalized-options-as-slots
	    (remove nil (mapcar #'canonicalize-option-as-slot
						   table-options)))
	  (canonicalized-defclass-options
	   (remove nil (mapcar #'canonicalize-option-as-defclass-option
						   table-options))))
       `(defclass ,table-name (,@canonicalized-supers)
	 (,@canonicalized-slots
	  ,@canonicalized-options-as-slots)
	 (:metaclass square-table-class)
	 ,@canonicalized-defclass-options)))

(define-test def-square-table-macro
    (assert-expands (defclass test-table ()
		      ((row-labels-schema :names (a b c)
					  :comparator #'char<
					  :allocation :class)
		       (col-labels-schema :names (1 2 3)
					  :allocation :class)
		       (value-normalizer :reader
					 value-normalizer
					 :initform #'floor
					 :allocation :class))
		      (:metaclass square-table-class)
		      (:documentation "Test table for unit tests"))
		  (def-square-table test-table ()
		    ((row-labels-schema :names (a b c)
					:comparator #'char<)
		     (col-labels-schema :names (1 2 3)))
		    (:documentation "Test table for unit tests")
		    (:value-normalizer #'floor))))


