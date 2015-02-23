(in-package :column-table)

(defclass column-accessor (column-table-slot-definition-mixin)
  ((column-name :initarg :column-name
		:reader column-name
		:documentation
		"Column name

Column-name must match (slot-definition-name column-schema)")
   (table :initarg table
	  :reader table
	  :documentation
	  "The table that the column-accessor is associated with")
   (table-schema :reader associated-table-schema
		 :documentation
		 "The table schema, as defined with `defclass' or `deftable'")
   (column-schema :reader associated-column-schema
		  :documentation
		  "The column schema.  This is an object of class
column-table-effective-slot-defintion"))
  (:documentation "Accessor to a table column.


It has links to the table schema, column schema, the table.

It also contains all the slots that were used to define the
column-schema
"))

(defmacro awhen-slot-bound-p ((slot class) &body body)
  "Execute body if Class' slot is bound.

Bind it to the value of slot"
  `(when (slot-boundp ,class ',slot)
     (let ((it (slot-value ,class ',slot)))
       ,@body)))

(defmethod initialize-instance :after ((self column-accessor)
				       &key table &allow-other-keys)
  (let* ((table-schema (table-schema table))
	 (column-schema (column-schema  (column-name self) table)))
    ;; set meta-data
    (setf (slot-value self 'table) table
	  (slot-value self 'table-schema) table-schema
	  (slot-value self 'column-schema) column-schema)
    ;; copy default column definitions into the column accessor
    (setf (slot-value self 'sequence-type) (sequence-type column-schema)
	  (slot-value self 'action) (action column-schema)
	  (slot-value self 'storage) (storage column-schema)
	  (slot-value self 'value-normalizer) (value-normalizer column-schema))
    ;; copy optional column definition into the column accessor if
    ;; defined
    (awhen-slot-bound-p (equality-predicate column-schema)
      (setf (slot-value self 'equality-predicate) it))
    (awhen-slot-bound-p (comparator column-schema)
      (setf (slot-value self 'comparator) it))
    (awhen-slot-bound-p (null-value column-schema)
      (setf (slot-value self 'null-value) it))
    (awhen-slot-bound-p (default-value column-schema)
      (setf (slot-value self 'default-value) it))
    (awhen-slot-bound-p (lazy-p column-schema)
      (setf (slot-value self 'lazy-p) it))))

(define-test column-accessor-instantiation
  "Test that the column has access to the table class and the column schema"
  (:tag :column-table :column-accessor)
  (let* ((table (let ((schema (defclass test-table ()
				((column-0 :action :read)
				 (column-1 :sequence-type :list))
				(:metaclass column-table))))
		  (make-instance 'test-table))))
    (let* ((column-accessor-0 (make-instance 'column-accessor
					     :column-name 'column-0
					     :table table))
	   (column-schema (associated-column-schema column-accessor-0)))
      (assert-equal 'column-0 (column-schema-name column-schema))
      (assert-equal 'column-0 (column-name column-accessor-0)
		    "Check column-accessor's column-name")
      (assert-equal table (table column-accessor-0)
		    "Check table stored correctly"))))

#+skip(defgeneric column-action (accessor)
  (:documentation "Column's action value")
  (:method ((self column-accessor))
    (action (associated-column-schema self))))

(defgeneric column-read-p (accessor)
  (:documentation "Return true if column is read or read+write")
  (:method ((self column-accessor))
    (or (find (print (action (associated-column-schema self)))
	    '(:read :read+write)))))

(defgeneric column-write-p (accessor)
  (:documentation "Return true if column is write or read+write")
  (:method ((self column-accessor))
    (or (find (action (associated-column-schema self))
	    '(:read :read+write)))))



(defgeneric column-reader (accessor)
  (:documentation "return a function that can return the column
  contents in internal format")
  (:method ((self column-accessor))
    (let* ((column-schema (associated-column-schema self))
	   (raw-data-reader (raw-data-reader (storage column-schema)))
	   (raw-data-source (raw-data-source (data-source column-schema))))
      (raw-data function))))


(defgeneric raw-data-reader (storage sequence-type)
  (:method ((storage stream) (sequence-type sequence))
    #'read)
  (:method ((storage t) (sequence-type t))
    #'slot-value))

(defgeneric raw-data-source (storage  accessor)
  (:method ((storage stream)  accessor)
    (declare (ignore accessor))
    storage)
  (:method ((storage t) accessor)
    (table accessor)))

(defun raw-data (function source &rest rest)
  "Retrieve data in the raw format for column-accessor"
  (apply function source rest))

(defgeneric call-op (action argument &key)
  (:documentation "Generic operator"))

(defgeneric coerce-sequence (data format accessor &key)
  (:documentation "Coerce the internal data sequence to requested format"))

;;(defmethod call-op (action (accessor column-accessor) &key))

(defgeneric contents (accessor &key)
  (:documentation "access column contents via column accessor")
  (:method ((accessor column-accessor) &key (format 'vector))
    (let ((name (column-name accessor))
	  (column-schema (associated-column-schema accessor)))
      (let* ((reader (when (column-read-p accessor)
				(raw-data-reader (storage column-schema) accessor)))
	     (source (raw-data-source (storage column-schema) accessor))
	     (internal-format-data (funcall reader source name)))
	internal-format-data
	#+skip	(coerce-sequence internal-format-data format accessor)))))

(defgeneric (setf contents) (value accessor &key)
  (:documentation "set column contents via column accessor")
  (:method (value (accessor column-accessor) &key (format 'vector))
    (let ((name (column-name accessor))
	  (column-schema (associated-column-schema accessor)))
      (let* ((writer (when (column-write-p accessor)
		       (raw-data-source (storage column-schema) accessor)))
	     (source (raw-data-source (storage column-schema) accessor))
	     (internal-format-data (funcall writer source name)))
	internal-format-data
	#+skip	(coerce-sequence internal-format-data format accessor)))))



#|
(slot-value self 'name)
(read stream)
(read-binary-format-xyz stream)

|#

(define-test column-table-contents
  "Setting and retrieving column table contents"
  (:tag :column-table :column-accessor)
  (let* ((schema (defclass test-table ()
		   ((column-0))
		   (:metaclass column-table)))
	 (table (make-instance 'test-table))
	 (column-accessor (make-instance 'column-accessor
					 :column-name 'column-0
					 :table table)))
    (print (class-of table))
    (print (class-name (class-of table)))
    (setf (slot-value table 'column-0) 'a)
    (assert-equal 'a (contents column-accessor))
    #+skip (setf (contents column-accessor) 'b)
    #+skip (assert-equal 'b (slot-value table 'column-0))))
  


(define-test redef-slot-value
  (defclass test-meta (standard-class)
    ())
  (defmethod validate-superclass ((self test-meta) (super standard-class))
    t)
  (defmethod (setf slot-value-using-class) :around (value (class test-meta)
							  object slot-name)
	     (print :around-setf-slot-value-using-class)
	     (call-next-method))
  (defclass test-class ()
	     ((slot-a))
	     (:metaclass test-meta))
  (let ((a (make-instance 'test-class)))
    (setf (slot-value a 'slot-a) 1)
    (slot-value a 'slot-a)))

(defmethod (setf slot-value-using-class) :around
    (value (class column-table) table column-name)
  (break)
  (print :around-slot-value-using-class)
  (call-next-method))


