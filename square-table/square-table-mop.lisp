(in-package :table-dev)

(defclass square-table-class (standard-class)
  ((value-type :initarg :value-type
	       :accessor value-type)
   (value-normalizer :initarg :value-normalizer
		     :accessor value-normalizer)
   (equality-predicate :initarg :equality-predicate
		       :accessor equality-predicate)
   (comparator :initarg :comparator
	       :accessor comparator)
   (default-value :initarg :default-value
     :accessor default-value)
   (adjustable :initarg :adjustable
	       :reader adjustable)))

(defmethod validate-superclass
    ((class square-table-class) (superclass standard-class))
  t)

#+skip(defmethod ensure-class-using-class ((class square-table-class)
				     table-name
				     &rest keys
				     &key metaclass
				       direct-superclasses
				       ;; These correspond to table properties
				       (value-type nil value-type-p)
				       (value-normalizer nil value-normalizer-p)
				       (equality-predicate nil equality-predicate-p)
				       (comparator nil comparator-p)
				       (default-value nil default-value-p)
				       (adjustable nil adjustable-p)
				       &allow-other-keys)
  (declare (ignore value-type value-normalizer
		   equality-predicate comparator
		   default-value adjustable))
  (let ((i-keys keys))
    (remf i-keys metaclass)
    (remf i-keys direct-superclasses)
    (unless value-type-p
      (remf i-keys :value-type))
    (unless value-normalizer-p
      (remf i-keys :value-normalizer))
    (unless equality-predicate-p
      (remf i-keys :equality-predicate))
    (unless comparator-p
      (remf i-keys :comparator))
    (unless default-value-p
      (remf i-keys :default-value))
    (unless adjustable-p
      (remf i-keys :adjustable))
    (apply #'make-instance 'square-table-class
	   :class-name table-name
	   :metaclass metaclass
	   :direct-superclasses direct-superclasses
	   i-keys)))

(defclass schema-slot-mixin% ()
  ((title :reader title)
   (names :reader names)
   (equality-predicate :reader equality-predicate)
   (comparator :reader comparator)
   (value-normalizer :reader value-normalizer))
  (:documentation "Defines additional options for the slots that will
  store the schema

This class is meant to be inherited, never instantiated by itself"))

(defclass schema-direct-slot-definition
    (standard-direct-slot-definition schema-slot-mixin%)
  ()
  (:documentation "The additional slot options are loaded via the mixin"))

(defmethod direct-slot-definition-class ((class square-table-class)
					 &rest initargs
					 &key name)
  (declare (ignore initargs))
  (if (or (eql name 'table-rows-schema)
	  (eql name 'table-cols-schema))
      (find-class 'schema-direct-slot-definition)
      (find-class 'standard-direct-slot-definition)))

(defclass schema-effective-slot-definition
    (standard-effective-slot-definition schema-slot-mixin%)
  ()
  (:documentation "Additional slot options are loaded via the mixin"))

(defmethod effective-slot-definition-class ((class square-table-class)
					    &rest initargs
					    &key name)
  (declare (ignore initargs))
  (if (or (eql name 'table-rows-schema)
	  (eql name 'table-cols-schema))
      (find-class 'schema-effective-slot-definition)
      (find-class 'standard-effective-slot-definition)))

(defmethod compute-effective-slot-definition ((class square-table-class)
					      slot-name
					      direct-slot-definitions)
  "Compute inheritance for each slot.  We do additional processing for
TABLE-ROWS-SCHEMA and TABLE-COLS-SCHEMA:

- We append all the names, and if the comparator function is
  defined,we remove duplicates

- For all other properties (title, comparator, equality-predicate,
  value-normalizer, we exclusively use the values from the most
  specific schema"
  (let ((effective-slot-definition (call-next-method)))
    (when (or (eql slot-name 'table-rows-schema)
	      (eql slot-name 'table-cols-schema))
      (let ((effective-names (mappend #'names direct-slot-definitions))
	    (first-direct-slot-definition (first direct-slot-definitions)))
	(with-slots (equality-predicate comparator title value-normalizer)
	    first-direct-slot-definition
	(when (slot-boundp first-direct-slot-definition 'equality-predicate)
	  (setf effective-names
		(remove-duplicates effective-names :test equality-predicate)))
	(with-slots (equality-predicate-1 comparator-1 title-1
					  names-1 value-normalizer-1)
	    effective-slot-definition
	  (setf names-1 effective-names)
	  (when (slot-boundp first-direct-slot-definition 'equality-predicate)
	    (setf equality-predicate-1 equality-predicate))
	  (when (slot-boundp first-direct-slot-definition 'comparator)
	    (setf comparator-1 comparator))
	  (when (slot-boundp first-direct-slot-definition 'title)
	    (setf title-1 title))
	  (when (slot-boundp first-direct-slot-definition 'value-normalizer)
	    (setf value-normalizer-1 value-normalizer))))))
    effective-slot-definition))

(defmethod compute-slots ((class square-table-class))
  "We compute slots, and return them.  At some later st"
  (let ((normal-slots (call-next-method)))
    normal-slots))
