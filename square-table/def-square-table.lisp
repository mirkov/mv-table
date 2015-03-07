(in-package :table-dev)

;; this file deals only with the def-square-table macro.  It is a long
;; macro (according to CCL), and I want it cleanly documented.


(defun canonicalize-option (option options-seen table-options)
  "Return option in correct format.  Signal error for unrecognized
options"
  (let* ((option-name (car option))
	 (option-value
	  (let ((value (cadr option)))
	    (assert value ()
		    (error 'table-option-missing-value :option-name option-name))
	    value)))
    (check-duplicate-option option-name options-seen table-options)
    (let ((table-option-maybe
	   (canonicalize-deftable-option option-name option-value)))
      (if table-option-maybe table-option-maybe
	  (let ((class-option-maybe
		 (canonicalize-defclass-option option-name option-value)))
	    (if class-option-maybe class-option-maybe
		(error 'illegal-table-option :name option-name)))))))
		 
		 

(defun check-duplicate-option (option-name options-seen table-options)
  "Check for duplicate options in options-seen"
  (when (member option-name options-seen :test #'eq)
	(error 'duplicate-table-option :options table-options)))

(defun canonicalize-deftable-option (option-name option-value)
  "Check table options and bring them to correct form.  Return NIL if
  not a table-option"
    (macrolet ((assert-function ()
		 `(assert (functionp option-value) ()
			  (error 'table-option-not-function
				 :option-name option-name))))
      (case option-name
	(:value-type 
		     `(:value-type ,option-value))
	(:value-normalizer (assert-function)
	 `(:value-normalizer ,option-value))
	(:equality-predicate (assert-function)
			     `(:equality-predicate ,option-value))
	(:comparator (assert-function)
	 `(:comparator ,option-value))
	(:default-value 
	 `(:default-value ,option-value))
	(:adjustable 
	 `(:adjustable ,option-value))
	(t nil))))

(defun canonicalize-defclass-option (option-name option-value)
  "Process class options:
- :metaclass is not allowed and we signal an error
- :default-initargs is not handled, we signal an error.  This needs to
  be fixed in a sensible manner by incorpoating ccl code
- :documentation is handled"
    (case option-name
      (:default-initargs
       (error 'illegal-table-option :name option-name))
      (:metaclass
       (error 'illegal-table-option :name option-name))
      (:documentation
       `(:documentation ,option-value))
      (t nil)))





(defmacro def-square-table (table-name table-supers slots
			    &rest table-options
			    &environment env)
  "Prepare to call ensure-class-using-class"
  (declare (ignore slots env))
  (assert (and table-name
	       (symbolp table-name)) ()
	       (error 'invalid-table-name :name table-name))
  (dolist (table-super table-supers)
    (assert (and table-super
	       (symbolp table-super)) ()
	       (error 'invalid-table-name :name table-super)))
    `(let ((direct-slot-specs nil)
	   (options ,(mapcar #'canonicalize-deftable-option table-options)))
       (apply #'closer-mop:ensure-class-using-class
	      ,(find-class table-name nil)
	      ,table-name
	     :direct-superclasses ,table-supers
	     :direct-slots ,canonicalized-slots
	     :direct-default-initargs nil
	     :metaclass nil
	     :documentation documentation
	     :value-type value-type 
	:value-normalizer value-normalizer
	:equality-predicate equality-predicate 
	:comparator comparator 
	:default-value default-value 
	:adjustable adjustable)))


(defmethod ensure-class-using-class ()

      `(ensure-class-using-class 
				  ',table-name
				:direct-superclasses ',table-supers
				:direct-slots ,`(list ,@direct-slot-specs)
				,@options)
  
