(in-package :table-dev)

(defclass square-table-storage ()
  ((2d-structure :accessor 2d-structure)))

(defclass square-table-array (square-table-storage)
  ())

(defclass square-table-foreign-array (square-table-storage)
  ())

(defclass square-table-hash^2 (square-table-storage)
  ((default-value :initarg :default-value)
   (value-type :initarg value-type)))

(defmethod initialize-instance :after ((self square-table-array)
				       &key num-cols
					 num-rows
					 (default-value nil default-value-p)
					 (value-type nil value-type-p)
					 &allow-other-keys)
  (let (kwd-arguments)
    (when default-value-p
      (setf (getf kwd-arguments :initial-element) default-value))
    (when value-type-p
      (setf (getf kwd-arguments :element-type) value-type))
    (setf (2d-structure self)
	(apply #'make-array (list num-cols num-rows)
	       kwd-arguments))))

(defmethod initialize-instance :after ((self square-table-foreign-array)
				       &key num-cols
					 num-rows
					 (default-value nil default-value-p)
					 (value-type nil value-type-p)
					 &allow-other-keys)
  (assert value-type-p ()
	  (error 'unspecified-value-type))
  (let (kwd-arguments)
    (when default-value-p
      (setf (getf kwd-arguments :initial-element) default-value))
    (when value-type-p
      (setf (getf kwd-arguments :element-type) value-type))
    (setf (2d-structure self)
	(apply #'make-foreign-array value-type
	       :dimensions (list num-cols num-rows)
	       kwd-arguments))))


(defmethod initialize-instance :after ((self square-table-hash^2)
				       &key &allow-other-keys)
  (setf (2d-structure self)
	(make-hash-table)))


;;; Row and column identifiers
(defmethod col-identifier ((2d-structure square-table-array)
			   (schema col-labels-schema)
			   (col-name integer))
  col-name)

(defmethod col-identifier ((2d-structure square-table-foreign-array)
			   (schema col-labels-schema)
			   (col-name integer))
  col-name)

(defmethod col-identifier ((2d-structure square-table-array)
			   (schema col-labels-schema)
			   (col-name t))
  (position col-name (names schema)
	    :test (equality-predicate schema)))

(defmethod col-identifier ((2d-structure square-table-foreign-array)
			   (schema col-labels-schema)
			   (col-name t))
  (position col-name (names schema)
	    :test (equality-predicate schema)))

(defmethod col-identifier ((2d-structure square-table-hash^2)
			   (schema col-labels-schema)
			   (col-name t))
  col-name)


(defmethod row-identifier ((2d-structure square-table-array)
			   (schema row-labels-schema)
			   (row-name integer))
  row-name)

(defmethod row-identifier ((2d-structure square-table-foreign-array)
			   (schema row-labels-schema)
			   (row-name integer))
  row-name)

(defmethod row-identifier ((2d-structure square-table-array)
			   (schema row-labels-schema)
			   (row-name t))
  (position row-name (names schema)
	    :test (equality-predicate schema)))

(defmethod row-identifier ((2d-structure square-table-foreign-array)
			   (schema row-labels-schema)
			   (row-name t))
  (position row-name (names schema)
	    :test (equality-predicate schema)))

(defmethod row-identifier ((2d-structure square-table-hash^2)
			   (schema row-labels-schema)
			   (row-name t))
  row-name)

;;; Accessors
(defmethod aref ((self square-table-array) &rest indices)
  (destructuring-bind (row-index col-index) indices
    (aref (slot-value self '2d-structure) row-index col-index)))

(defmethod (setf aref) (value (self square-table-array) &rest indices)
  (destructuring-bind (row-index col-index) indices
    (setf (aref (slot-value self '2d-structure) row-index col-index)
	  value)))

(defmethod aref ((self square-table-foreign-array) &rest indices)
  (destructuring-bind (row-index col-index) indices
    (aref (slot-value self '2d-structure) row-index col-index)))

(defmethod (setf aref) (value (self square-table-foreign-array) &rest indices)
  (destructuring-bind (row-index col-index) indices
    (setf (aref (slot-value self '2d-structure) row-index col-index)
	  value)))

(defmethod aref ((self square-table-hash^2) &rest names)
  "Returns the value and whether it was present in the hash

This is unlike other methods."
  (with-slots (default-value) self
    (destructuring-bind (col-name row-name) names
      (multiple-value-bind (col-hash col-present-p)
	  (gethash col-name (slot-value self '2d-structure))
	(if col-present-p
	    (multiple-value-bind (value value-present-p)
		(gethash row-name col-hash)
	      (if value-present-p (values value t)
		  (values default-value nil)))
	    (values default-value nil))))))

(defmethod (setf aref) (value (self square-table-hash^2) &rest names)
  "If the place is not present, we unconditionally create it."
  (with-slots (2d-structure) self
    (destructuring-bind (col-name row-name) names
      (multiple-value-bind (col-hash col-present-p)
	  (gethash col-name 2d-structure)
	(unless col-present-p
	  (setf col-hash
		(setf (gethash col-name 2d-structure) (make-hash-table))))
	(setf (gethash row-name col-hash) value)))))

