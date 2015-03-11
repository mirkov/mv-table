(in-package :table-dev)

;; this file deals only with the def-square-table macro.  It is a long
;; macro (according to CCL), and I want it cleanly documented.


(defun canonicalize-option (option)
  "Do some checks and return option in correct format.  Signal error for unrecognized or illegal options.

We check that options have values
We check that there are no duplicate options
We check that :metaclass has not been specified
We pass through unrecognized options

This is a top-level function that calls a few helper functions to do
its job"
  (let* ((option-name (car option))
	 (option-values
	  (let ((values (cdr option)))
	    (assert values ()
		    (error 'table-option-missing-value :option-name option-name))
	    values)))
    (check-duplicate-option option-name)
    (let ((table-option-maybe
	   (apply #'canonicalize-deftable-option option-name option-values)))
      (if table-option-maybe table-option-maybe
	  (let ((class-option-maybe
		 (apply #'canonicalize-defclass-option option-name option-values)))
	    (if class-option-maybe class-option-maybe
		`(,option-name ,@option-values)))))))
		 
		 

(defun check-duplicate-option (option-name)
  "Check for duplicate options in options-seen"
  (declare (special options-seen))
  (if (member option-name options-seen :test #'eq)
      (error 'duplicate-table-option :option-name option-name) 
      (push option-name options-seen)))

(defun canonicalize-deftable-option (option-name &rest option-values)
  "Check table options and bring them to correct form.  Return NIL if
  not a table-option"
    (macrolet ((assert-function ()
		 `(assert (functionp option-value) ()
			  (error 'table-option-not-function
				 :option-name option-name))))
      (case option-name
	(:value-type 
		     `(:value-type ,(car option-values)))
	(:value-normalizer #+skip(assert-function)
	 `(:value-normalizer ,(car option-values)))
	(:equality-predicate #+skip(assert-function)
			     `(:equality-predicate ,(car option-values)))
	(:comparator #+skip(assert-function)
	 `(:comparator ,(car option-values)))
	(:default-value 
	 `(:default-value ,(car option-values)))
	(:adjustable 
	 `(:adjustable ,(car option-values)))
	(t nil))))

(define-test canonicalize-deftable-option
  "Spot check of the functions main operation"
  (assert-equal '(:value-type integer)
		(canonicalize-deftable-option :value-type 'integer))
  #+skip(assert-error 'table-option-not-function
		(canonicalize-deftable-option :comparator t))
  (assert-true
		(canonicalize-deftable-option :comparator
					      (lambda (x y)
						t))))


(defun canonicalize-defclass-option (option-name &rest option-values)
  "Process class options:
- :metaclass is not allowed and we signal an error
- :default-initargs is not handled, we signal an error.  This needs to
  be fixed in a sensible manner by incorpoating ccl code
- :documentation is handled"
  (case option-name
    (:metaclass (error 'illegal-table-option :option-name option-name))
    #+skip(:default-initargs `(:default-initargs ,@option-values))
    (t `(,option-name ,@option-values))))

(define-test canonicalize-defclass-option
  (assert-error 'illegal-table-option
		(canonicalize-defclass-option :metaclass 'foo))
  (assert-equal '(:documentation "text")
		(canonicalize-defclass-option :documentation "text"))
  (assert-equal '(:default-initargs
		  :slot0 val0
		  :slot1 val1)
		(apply #'canonicalize-defclass-option :default-initargs
		       '(:slot0 val0 :slot1 val1))))


(define-test canonicalize-option
  "Check duplicate testing, and successful option processing"
  (declare (special options-seen table-options-1))
  (assert-equal '((:default-value 3)
		  (:documentation "text"))
		(progn
		  (setf table-options-1
			'((:default-value 3)
			  (:documentation "text"))
			options-seen nil)
		  (mapcar #'canonicalize-option table-options-1)))
  (assert-error 'duplicate-table-option
		(progn
		  (setf table-options-1
			'((:default-value 3)
			  (:documentation "text")
			  (:default-value 4))
			options-seen nil)
		  (mapcar #'canonicalize-option table-options-1)))
  (assert-error 'illegal-table-option
		(progn
		  (setf table-options-1
			'((:default-value 3)
			  (:metaclass 'foo))
			options-seen nil)
		  (mapcar #'canonicalize-option table-options-1))))


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
  (declare (special slot-options-seen))
  (destructuring-bind (slot-name &rest slot-options) slot
    (assert slot-name ()
	    (error 'illegal-table-slot-name :slot-name slot-name))
    #+clos-does-not-handle-duplicate-slots(check-duplicate-slot slot-name)
    (setf slot-options-seen nil)
    (do* ((options slot-options (cddr options))
	  (option-name (car options) (car options)))
	((null options))
      (check-duplicate-slot-option option-name slot-name)
      (unless (cdr options)
	(error 'missing-slot-option-value :slot-name slot-name
	       :slot-option-name (car options))))
    (destructuring-bind (slot-name &rest slot-options) slot
      (canonicalize-table-slot-by-name slot-name slot-options))))

;; we leave to CLOS to handle duplicate slots
#+skip(defun check-duplicate-slot (slot-name)
  "Check for duplicate slots in slots-seen"
  (declare (special slots-seen))
  (if (member slot-name slots-seen :test #'eq)
      (error 'duplicate-table-slot :slot-name slot-name) 
      (push slot-name slots-seen)))

;; we leave to CLOS to handle duplicate slot-options
(defun check-duplicate-slot-option (option-name slot-name)
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


(defun canonicalize-table-slot-by-name (name options)
  "Canonicalize table slot"
  (when (or (eql name 'row-labels-schema)
	      (eql name 'col-labels-schema))
    (assert (find :names options) ()
	    (error 'missing-table-slot-option :slot-option-name :names
		   :slot-name name)))
  `(,name ,@options))

(define-test canonicalize-table-slot-by-name
  (assert-equal '(row-labels-schema :names (a b c))
		(canonicalize-table-slot-by-name
		 'row-labels-schema (list :names '(a b c))))
  (assert-equal '(row-labels-schema :names (a b c) :default-value 3.14)
		(canonicalize-table-slot-by-name
		 'row-labels-schema
		 '(:names (a b c)
		   :default-value 3.14)))  
  (assert-error 'missing-table-slot-option
		(canonicalize-table-slot-by-name
		 'row-labels-schema (list :name '(a b c))))
  #+not-implemented(assert-error 'missing-table-slot-option-value 
		(canonicalize-table-slot-by-name
		 'row-labels-schema
		 '((:names (a b c))
		   (:default-value))))
  #+not-implemented(assert-error 'illegal-table-slot-option-value
		(canonicalize-table-slot-by-name
		 'row-labels-schema
		 '(:names (a b c)
		   :default-value 3.14
		   :equality-predicate 2.71)))
)
  
(define-test canonicalize-table-slot
  (declare (special slots-seen))
  (assert-equal '(row-labels-schema :names (a b c) :default-value 3.14)
		(progn
		  (setf slots-seen nil)
		  (canonicalize-table-slot '(row-labels-schema :names (a b c)
					     :default-value 3.14))))
  (assert-error 'duplicate-table-slot-option
		(progn
		  (setf slots-seen nil
			options-seen nil)
		  (canonicalize-table-slot '(row-labels-schema :names (abc)
					     :default-value 3.14
					     :default-value 2.71)))))




(defmacro def-square-table (table-name table-supers slots
			    &rest table-options
			    &environment env)
  "prepare to call ensure-class-using-class"
  (declare (ignore env))
  (declare (special options-seen slots-seen))
  (assert (and table-name
	       (symbolp table-name)) ()
	       (error 'invalid-table-name :name table-name))
  (setf options-seen nil
	slots-seen nil)
    (let ((canonicalized-supers (mapcar #'canonicalize-table-super table-supers))
	   (canonicalized-slots (mapcar #'canonicalize-table-slot slots))
	   (canonicalized-options (mapcar #'canonicalize-option table-options)))
       `(defclass ,table-name (,@canonicalized-supers)
	 (,@canonicalized-slots)
	 (:metaclass square-table-class)
	 ,@canonicalized-options)))

(define-test def-square-table-macro
    (assert-expands (defclass test-table ()
		    ((row-labels-schema :names (a b c)
					:comparator #'char<)
		     (col-labels-schema :names (1 2 3)))
		    (:metaclass square-table-class)
		    (:documentation "Test table for unit tests")
		    (:value-normalizer #'floor))
		  (def-square-table test-table ()
		    ((row-labels-schema :names (a b c)
					:comparator #'char<)
		     (col-labels-schema :names (1 2 3)))
		    (:documentation "Test table for unit tests")
		    (:value-normalizer #'floor)))
  (assert-expands (defclass test-table ()
		    ((row-labels-schema :names (a b c)
					:comparator #'char<)
		     (col-labels-schema :names (1 2 3)))
		    (:metaclass square-table-class)
		    (:documentation "Test table for unit tests")
		    (:value-normalizer #'floor))
		  (def-square-table test-table ()
		    ((row-labels-schema :names (a b c)
					:comparator #'char<)
		     (col-labels-schema :names (1 2 3)))
		    (:documentation "Test table for unit tests")
		    (:value-normalizer #'floor)))
  #+test-does-not-capture-error
  (assert-error 'duplicate-table-slot-option 
		  (def-square-table test-table ()
		    ((row-labels-schema :names (a b c)
					:comparator #'char<
					:comparator #'char>)
		     (col-labels-schema :names (1 2 3)))
		    (:documentation "Test table for unit tests")
		    (:value-normalizer #'floor))))


