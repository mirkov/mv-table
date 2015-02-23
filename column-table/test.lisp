(in-package :cl-user)

(defclass column-table-slot-definition-mixin-2 ()
  (#+skip(sequence-type :initarg :sequence-type
		:reader sequence-type
    :documentation "Column storage method  

Allowed values are :list or :simple-vector.  :list is for illustration
purposes, default is :simple-vector.  Other methods may be added by
inheritance.")
   #+skip(action :initarg :action
	   :reader action
    :documentation ":read, :write, or :read+write")
   #+skip(storage :initarg :storage
		 :reader storage
    :documentation "Where does the column data reside?

Allowed values are t, :file or :memory, default is t.  Other types can
be added by inheritance.

If t, the storage is specified by the table.  Setting it to other
values over-rides the setting by the table.")
   #+skip(value-normalizer :initarg :value-normalizer
		     :reader value-normalizer
    :documentation "A function of two arguments (value, column schema
    -what exactly do I mean by that?)  that normalizes the value prior
    to storing into column. ")
   #+skip(equality-predicate :initarg :equality-predicate
		       :reader equality-predicate
		       :documentation
"A function of two arguments that tests column values for equality")
   #+skip(comparator :initarg :comparator
	       :reader comparator
	       :documentation
"A function of two arguments that is used to order column values")
   #+skip(null-value :initarg :null-value
	       :reader null-value
	       :documentation "Value that signifies un-assigned column value")
   #+skip(default-value :initarg :default-value
     :reader default-value
     :documentation "When initializing a column, it is set to default value")
   #+skip(lazy-p :initarg :lazy-p
	   :reader lazy-p
	   :documentation
"For columns whose contents are a result of a pre-defined action other
then (setf ... sequence), if true, execute action when a value is
requested from the column.  This applies, for example to the
application of the :initarg form.

Default is nil "))
  (:documentation "Mixing storing the column table meta-class slot definition.

This mixin is used by direct & effective slot definition classes")
  #+skip(:default-initargs
   :action :read+write
    :storage t
    :value-normalizer #'(lambda (arg &optional schema)
			  (declare (ignore schema))
			  arg)
    :sequence-type :simple-vector
    :lazy-p nil))


(defclass column-table-slot-definition-mixin-3 ()
  ()
  (:documentation "Mixing storing the column table meta-class slot definition.

This mixin is used by direct & effective slot definition classes"))
(defclass test (column-table-definition-mixin-3)
  ())

(make-instance 'test)
