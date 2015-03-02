(in-package :table-dev)

(defclass square-table (table-format)
  ((row-labels-schema :initarg :row-labels-schema
		:accessor row-labels-schema)
   (col-labels-schema :initarg :col-labels-schema
		   :accessor col-labels-schema)
   (value-type :initarg :value-type
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
	       :reader adjustable)
   (data-matrix ;;:initarg :data-matrix
	    :reader data-matrix
	    :documentation "Stores the data

The type of data structure in this slot depends on the storage
definition which is handled by initialize-instance"))
  (:default-initargs
   :default-value nil
    :value-type t
    :adjustable t
    :value-normalizer #'pass-through
    :equality-predicate #'equalp))


(defmethod initialize-instance :after ((self square-table) &key storage)
  (let ((kwd-parameters (list :num-rows (num-rows (row-labels-schema self))
			      :num-cols (num-cols (col-labels-schema self)))))
    (when (slot-boundp self 'adjustable)
      (setf (getf kwd-parameters :adjustable) t))
    (when (slot-boundp self 'default-value)
      (setf (getf kwd-parameters :default-value)
	    (slot-value self 'default-value)))
    (when (slot-boundp self 'value-type)
      (setf (getf kwd-parameters :value-type)
	    (slot-value self 'value-type)))
    (setf (slot-value self 'data-matrix)
	  (apply #'make-instance storage kwd-parameters))))


(defun make-square-table (row-labels-schema col-labels-schema
		   &rest keys
		   &key default-value value-type value-normalizer adjustable
		     (storage *square-table-default-storage*) 
		     (device *default-device*)
		     (protocol *default-protocol*) (direction *default-direction*))
  (declare (ignore default-value value-type value-normalizer adjustable))
  #+skip(assert (subtypep (type-of row-labels-schema)
		    'row-labels-schema) ()
		    "Type of ~a is not ROW-LABELS-SCHEMA" row-labels-schema)
  #+skip(assert (subtypep (type-of col-labels-schema)
		    'col-labels-schema) ()
		    "Type of ~a is not COL-LABELS-SCHEMA" col-labels-schema)
  (let ((table-class (find-programmatic-table-class
		      'square-table direction device protocol)))
    (remf keys 'device)
    (remf keys 'protocol)
    (remf keys 'direction)
    (apply #'make-instance table-class
	 :row-labels-schema row-labels-schema
	 :col-labels-schema col-labels-schema
	 :storage storage
	 keys)))


(defmethod num-rows ((table square-table))
  (num-rows (row-labels-schema table))) 

(defmethod num-cols ((table square-table))
  (num-cols (col-labels-schema table)))

(defmethod table-dimensions ((table square-table))
  (list (num-cols table) (num-rows table)))

(defmethod table-ref ((self square-table) (row-index integer) (col-index integer) &key)
  (with-slots (data-matrix col-labels-schema row-labels-schema)
      self
    (aref data-matrix row-index col-index)))

(defmethod table-ref ((self square-table) (row-name t) (col-name t) &key)
  (with-slots (data-matrix col-labels-schema row-labels-schema)
      self
    (table-ref self (name-index row-labels-schema row-name)
	       (name-index col-labels-schema col-name))))


(defmethod (setf table-ref) (value (self square-table)
			     (row-index integer) (col-index integer))
  "A lower level method for setting values.  It assumes that we have
  checked our indices, so no checking is performed.

In case of invalid or out-of-bound indices, we rely on the internal
storage objects to handle it (signal an error, or extend the storage)
"
  (with-slots (data-matrix col-labels-schema row-labels-schema)
      self
    (setf (aref data-matrix row-index col-index)
	  value)))

(defmethod (setf table-ref) (value (self square-table) (row-name t) (col-name t))
  (with-slots (data-matrix col-labels-schema row-labels-schema adjustable)
      self
    (let ((row-index
	   (let ((search-result (name-index row-labels-schema row-name)))
	     (unless search-result
	       (if adjustable
		   (setf search-result (add-name row-labels-schema row-name))
		   (error 'undefined-row :row-name row-name)))
	     search-result))
	  (col-index
	   (let ((search-result (name-index col-labels-schema col-name)))
	     (unless search-result
	       (if adjustable
		   (setf search-result (add-name col-labels-schema col-name))
		   (error 'undefined-col :col-name col-name))) 
	     search-result)))
      (setf (table-ref self row-index col-index) value))))



