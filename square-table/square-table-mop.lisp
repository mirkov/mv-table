(in-package :table-dev)

(defclass square-table-class (standard-class)
  (   #+skip(value-normalizer :initarg :value-normalizer
		     :accessor value-normalizer)
#|(data-matrix :reader data-matrix)
   (row-count :reader :row-count)
   (col-count :reader :col-count)
   (value-type :initarg :value-type
	       :accessor value-type)

   (equality-predicate :initarg :equality-predicate
		       :accessor equality-predicate)
   (comparator :initarg :comparator
	       :accessor comparator)
   (default-value :initarg :default-value
     :accessor default-value)
   (adjustable :initarg :adjustable
	       :reader adjustable)|#)
  (:documentation "Metaclass for instances of 'square-table"))

(defmethod validate-superclass
    ((class square-table-class) (superclass standard-class))
  "We insist that table superclasses may be only superclassed"
  #+breaks-on(break)
  t #+skip(typep superclass 'square-table-class))

(defclass schema-slot-mixin% ()
  ((title :reader title)
   (names :reader names :initarg :names)
   (equality-predicate :reader equality-predicate
		       :initarg :equality-predicate)
   (comparator :reader comparator
	       :initarg :comparator)
   (value-normalizer :reader value-normalizer
		     :initarg :value-normalizer))
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
  #+breaks-on(break)
  (print name)
  (find-class
   (print (if (or (eql name 'row-labels-schema)
		  (eql name 'col-labels-schema))
	      'schema-direct-slot-definition
	      'standard-direct-slot-definition))))

(defclass schema-effective-slot-definition
    (standard-effective-slot-definition schema-slot-mixin%)
  ()
  (:documentation "Additional slot options are loaded via the mixin"))

(defmethod effective-slot-definition-class ((class square-table-class)
					    &rest initargs
					    &key name)
  (declare (ignore initargs))
  #+breaks-on(break)
  (print name)
  (find-class
   (print (if (or (eql name 'row-labels-schema)
		  (eql name 'col-labels-schema))
	      'schema-effective-slot-definition
	      'standard-effective-slot-definition))))

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
  #+breaks-on(break)
  (let ((effective-slot-definition (call-next-method)))
    (when (or (eql slot-name 'row-labels-schema)
	      (eql slot-name 'col-labels-schema))
      #+breaks-on(break)
      (let ((effective-names (mappend #'names direct-slot-definitions))
	    (first-direct-slot-definition (first direct-slot-definitions)))
	(with-slots (equality-predicate comparator title value-normalizer)
	    first-direct-slot-definition
	(when (slot-boundp first-direct-slot-definition 'equality-predicate)
	  (setf effective-names
		(remove-duplicates effective-names :test equality-predicate)))
	(with-slots ((equality-predicate-1 equality-predicate)
		     (comparator-1 comparator)
		     (title-1 title)
		     (names-1 names)
		     (value-normalizer-1 value-normalizer))
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
  "We compute slots, and return them"
  #+breaks-on(break)
  (let ((normal-slots (call-next-method))
	(data-matrix-slot
	 (make-instance 'standard-effective-slot-definition
			:name 'data-matrix
			:documentation "The data structure that stores the data")))
    #+break-on(break)
    (cons data-matrix-slot normal-slots)))

(defmethod allocate-instance ((class square-table-class) &rest initargs)
  (let ((instance (call-next-method)))
    #+break-on(break)
    instance))

(defmethod allocate-instance ((class schema-direct-slot-definition)
				      &rest initargs)
  (let ((instance (call-next-method)))
    #+break-on(break)
    instance))
