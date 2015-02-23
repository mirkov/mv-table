(in-package :column-table)

(defclass column-table (standard-class)
  ()
  (:documentation "Meta-class for column tables"))

(defmethod validate-superclass ((self column-table) (super standard-class))
  t)

;; additional options for slot/column definitions.  See
;; user-documentation for specification

(defvar *column-table-meta-class-slot-names* '(sequence-type action
  storage value-normalizer equality-predicate comparator null-value
  default-value lazy-p)
  "Slot names of the column table meta class.  This list has to be
congruent to the slot names in column-table-slot-definition-mixin
class")

(defclass column-table-slot-definition-mixin ()
  ((sequence-type :initarg :sequence-type
		:reader sequence-type
    :documentation "Column storage method  

Allowed values are :list or :simple-vector.  :list is for illustration
purposes, default is :simple-vector.  Other methods may be added by
inheritance.")
   (action :initarg :action
	   :reader action
    :documentation ":read, :write, or :read+write")
   (storage :initarg :storage
		 :reader storage
    :documentation "Where does the column data reside?

Allowed values are :memory (default) or :file .  Other types can
be added by the user.

If t, the storage is specified by the table.  Setting it to other
values over-rides the setting by the table.")
   (value-normalizer :initarg :value-normalizer
		     :reader value-normalizer
    :documentation "A function of two arguments (value, column schema
    -what exactly do I mean by that?)  that normalizes the value prior
    to storing into column. ")
   (equality-predicate :initarg :equality-predicate
		       :reader equality-predicate
		       :documentation
"A function of two arguments that tests column values for equality")
   (comparator :initarg :comparator
	       :reader comparator
	       :documentation
"A function of two arguments that is used to order column values")
   (null-value :initarg :null-value
	       :reader null-value
	       :documentation "Value that signifies un-assigned column value")
   (default-value :initarg :default-value
     :reader default-value
     :documentation "When initializing a column, it is set to default value")
   (lazy-p :initarg :lazy-p
	   :reader lazy-p
	   :documentation
"For columns whose contents are a result of a pre-defined action other
then (setf ... sequence), if true, execute action when a value is
requested from the column.  This applies, for example to the
application of the :initarg form.

Default is nil "))
  (:documentation "Mixing storing the column table meta-class slot definition.

This mixin is used by direct & effective slot definition classes")
  (:default-initargs
   :action :read+write
    :storage :memory
    :value-normalizer #'(lambda (arg &optional schema)
			  (declare (ignore schema))
			  arg)
    :sequence-type :simple-vector
    :lazy-p nil))

(defclass column-table-direct-slot-definition (standard-direct-slot-definition
					       column-table-slot-definition-mixin)
  ()
  (:documentation "Direct slot definition for the column table"))

#+skip(defmethod initialize-instance :after ((self column-table-direct-slot-definition)
				       &key)
		 ;; placeholder for now
		 )

(defmethod direct-slot-definition-class ((class column-table)
					 &key &allow-other-keys)
  (find-class 'column-table-direct-slot-definition))

(defclass column-table-effective-slot-definition
    (standard-effective-slot-definition
     column-table-slot-definition-mixin)
  ()
  (:documentation
"Effective slot definition for the column table"))

(defmethod effective-slot-definition-class ((class column-table)
					    &key &allow-other-keys)
  (find-class 'column-table-effective-slot-definition))


(defun collect-slot-values (slot-name direct-slots)
  "Go through direct slots, and collect all bound values of slot-name

Return list of bound slot values or nil if none were found"
  (loop
     :for d-slot :in direct-slots
     :when (slot-boundp d-slot slot-name)
     :collect (slot-value d-slot slot-name) :into all-values
     :finally (return all-values)))

(defun compute-slot-definition (slot-name direct-slots);; (name list bound-p)
  "Given a slot name and list of values by inheritance, compute the
value that will go into the effective slot definition

The default is to take the value of the most specific definition"
  (let ((collected-values (collect-slot-values slot-name direct-slots)))
    (if collected-values
	(values
	 (cond
	   (t ;; return the most specific value
	    (first collected-values)))
	 t)
	(values nil nil))))

(defun subtype-all-p (list)
  (labels ((recurse (head rest flag)
	     (print '---)
	     (print head)
	     (print rest)
	     (print flag)
	     (cond
	       ((null flag) (return-from subtype-all-p nil))
	       ((null rest) flag)
	       (t (setf flag (print (subtypep head (car rest))))
		  (recurse (car rest) (cadr rest) flag)))))
    (recurse (car list) (rest list) t)))

(defmethod compute-effective-slot-definition ((class column-table)
					      name direct-slots)
  "For each slot, we compute the precedence list"
  (let ((effective-definition (call-next-method)))
    (loop
       :for slot-name :in *column-table-meta-class-slot-names*
       :do 
       (multiple-value-bind (value bound-p)
	   (compute-slot-definition slot-name direct-slots)
	 (when bound-p
	   (setf (slot-value effective-definition slot-name)
		 value))))
    (reduce #'subtypep (mapcar #'slot-definition-type direct-slots)
	    :initial-value t)
    effective-definition))

(defmethod compute-slots ((class column-table))
  "Two tasks
1 - Set-up storage (bucket) for column attributes defined in the direct
    and effective slot definition
2 - Set up access to the column schema

Both are stored in a single a-list.  

 ((column-name <slot-object>
               (attribute-name . value)
               (attribute-name . value)
                ...)
  (column-name ...)
  ...)

<slot-object> is the meta-object holding the
column definition, i.e, column-schema

The column schema is obtained as
 (second (assoc name (slot-value table-object 'column-schema-bucket)))

An attribute value is obtained as
 (cdr (assoc attribute-name
             (assoc attribute-name
                    (rest (rest (slot-value table-object
                                            'column-schema-bucket))))))"
  (let ((normal-slots (call-next-method)))
	    normal-slots))


(define-test deftable-creation
  "We shallowly test column-table meta-class creation and creation of
  column-tables

This is a very basic, first level test, that tries to catch glaring
errors in the MOP protocol.  Other tests will check the accuracy of
the table classes and table instances."
  (:tag :column-table :mop)
  (assert-true (defclass test-table-0 ()
		 ()
		 (:metaclass column-table))
	       "Test empty table initialization")
  (assert-true (defclass test-table-1 ()
		 ((column-0))
		 (:metaclass column-table))
	       "Test class initialization with a single, plain column")
  (assert-true (make-instance 'test-table-1)
	       "Test table instantiation")) 

(define-test deftable-correctness
  "We test the correctness of a table definition.  We want to check
  the values of the metaclass specification"
  (:tag :column-table :mop)
    (let* ((table
	    (progn
	      (defclass test-table-2 ()
		((column-0
		  :action :write))
		(:metaclass column-table))
	      (make-instance 'test-table-2)))
	   (class-of (class-of table))
	   (slot (first (class-slots class-of))))
      (assert-equal 'test-table-2 (class-name class-of))
      (assert-equal 'column-table-effective-slot-definition
		    (class-name (class-of slot)))
      (assert-equal :write (action slot) "Check that specification worked")
      (assert-true (storage slot) "Check default value for storage is T")))

(define-test deftable-inheritance
  "We test that the inheritance works properly.  We define a test-table-3"
  (:tag :column-table :mop)
  (let* ((table
	  (progn
	    (defclass test-table-3 (test-table-2)
	      ()
	      (:metaclass column-table))
	    (make-instance 'test-table-3)))
	 (class-of (class-of table))
	 (slot (first (class-slots class-of))))
    (assert-equal 'test-table-3 (class-name class-of))
    (assert-equal 'column-table-effective-slot-definition
		  (class-name (class-of slot)))
    (assert-equal :write (action slot))))


;;; Glue layer between the MOP definitions and the user

(defun table-schema (table)
  "Return the table schema -- this is the class of table"
  (class-of table))

(defun table-schema-name (table)
  "Return table schema name - this is the name used in defclass"
  (class-name (class-of table)))

(defun table-columns-schema (table)
  "Return list of schema of all columns of table

This is the objects that store the information in the slots"
  (class-slots (class-of table)))

(defun column-schema (name table)
  "Return the column schema for the table"
  (let ((table-columns-schema (table-columns-schema table)))
    (find name table-columns-schema :key #'slot-definition-name)))

(defun column-schema-name (column-schema)
  "The column name that the schema describes"
  (slot-definition-name column-schema))

(define-test schema-accessors
  "We test the interface to the table and column schema"
  (:tag :column-table :mop)
  (let* ((table (make-instance 'test-table-3))
	 (c-0-schema (column-schema 'column-0 table)))
    (assert-equal 'COLUMN-TABLE-EFFECTIVE-SLOT-DEFINITION
		  (class-name (class-of c-0-schema))
		  "Check class-name of the column-schema object")
    (assert-equal 'column-table (class-name (class-of (table-schema table))))
    (assert-equal 'test-table-3 (table-schema-name table))
    (assert-true c-0-schema)
    (assert-equal 'column-0 (column-schema-name c-0-schema))
    (assert-equal :simple-vector (sequence-type c-0-schema))
    (assert-equal :write (action c-0-schema))))
