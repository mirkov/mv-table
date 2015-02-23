(in-package :mv-table)


(defvar *legal-column-types*
  (list 'symbol 'number 'string)
  "List of allowed column types")

(defun add-allowed-column-types (new-type)
  "Add new-type to *legal-column-types*"
  (push new-type *legal-column-types*))

(defclass column-definition
           (standard-direct-slot-definition)
   ((name :initarg :name
	  :reader column-name
	  :documentation
"Column name, a symbol")
    (storage-type :initarg :storage-type
		  :reader storage-type
		  :documentation
"Storage type.  One of 'list 'array or 'grid:foreign-array")
    (column-element-type :initarg :column-element-type
		  :reader column-element-type
		  :documentation
"Element type stored in this column.  It must be one listed in
*allowed-column-types*")
    (comparator :initarg :comparator
		:reader column-comparator
		:documentation
"Function of two arguments that compares for sorting purposes")
    (equality-predicate :initarg :equality-predicate
			:reader column-equality-predicate
			:documentation
"Function of two arguments that tests for their equality")
    (default-value :initarg :default-value
      :reader column-default-value
      :documentation
"Default value for column")
    (unspecified-value :initarg :unspecified-value
		       :reader column-unspecified-value
		       :documentation
"Value that signals that this column element has not been specified")
    (value-normalizer :initarg :value-normalizer
		      :reader value-normalizer
		      :documentation
"Function of two arguments: an input value, and the object of the
column where the value is going.

This function is used to normalize an input value to an appropriate
type"))
    (:documentation "Class for specifying column properties")
    (:default-initargs
	:name (error "Must specify column name")
      :storage-type 'list))

(defmethod (setf column-element-type) :before (type (self column-definition))
  (declare (ignore self))
  (assert (member type *legal-column-types*) ()
	  "Type ~a is not member of *legal-column-types*" type))

(defmethod column-definition-class
    ((class table-meta-class) &key &allow-other-keys)
  (find-class 'column-definition))

(defun canonicalize-direct-columns (direct-columns)
  ;; modeled after MOP canonicalize-direct-slots, p. 286
  `(list ,@(mapcar #'canonicalize-direct-column direct-columns)))

(defun canonicalize-direct-column (spec)
  "Bring column specification into line with what
ensure-class-using-class will be able to handle"
  ;; pilfered from tAotMOP canonicalize-direct-slot, p. 286
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (car spec))
	    (storage-type nil)
	    (column-element-type nil)
	    (comparator nil)
	    (equality-predicate nil)
	    (default-value nil)
	    (unspecified-value nil)
	    (value-normalizer nil)
	    (other-options nil))
	(loop for (kwd value) on spec
	   :do (case kwd
		 (:storage-type (setf storage-type value))
		 (:column-element-type (setf column-element-type value))
		 (:comparator (setf comparator value))
		 (:equality-predicate (setf equality-predicate value))
		 (:default-value (setf default-value value))
		 (:unspecified-value (setf unspecified-value value))
		 (:value-normalizer (setf value-normalizer value))
		 (t (push-on-end kwd other-options)
		    (push-on-end value other-options))))
	`(list
	  :name ',name
	  ,@(when storage-type `(:storage-type ',storage-type))
	  ,@(when column-element-type `(:column-element-type ',column-element-type))
	  ,@(when comparator `(:comparator ',comparator))
	  ,@(when equality-predicate `(:equality-predicate ',equality-predicate))
	  ,@(when default-value `(:default-value ',default-value))
	  ,@(when unspecified-value `(:unspecified-value ',unspecified-value))
	  ,@other-options))))

(define-test canonicalize-direct-column
  "Testing of column direct options canonicalization"
  (:tag :column :mop)
  (assert-equal (canonicalize-direct-column 'test-name)))


#+skip(defclass column-type-effective-slot-definition
    (standard-effective-slot-definition)
  ((vector-type :initarg :vector-type :initform nil
		:accessor vector-type)))
