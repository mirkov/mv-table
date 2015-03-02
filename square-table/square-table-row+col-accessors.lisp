(in-package :table-dev)

(defclass square-table-row (table-row)
  ((table :accessor table
	  :initarg :table)
   (row-name :accessor row-name
	     :initarg :row-name)
   (row-index :accessor row-index
	      :initarg :row-index)
   (col-labels-schema :initarg :col-labels-schema
		      :accessor col-labels-schema)))


(defun make-square-table-row (table row-name)
  (make-instance 'square-table-row :table table
		 :row-name row-name
		 :row-index (name-index (row-labels-schema table) row-name)
		 :col-labels-schema (col-labels-schema table)))

(defmethod (setf row-name) :before (name (row square-table-row))
  (assert (find name (names (table row))) ()
	  (error 'non-existant-row-name :name name)))

(defmethod (setf row-name) :after (name (row square-table-row))
  (setf (slot-value row 'row-index)
	(name-index (row-labels-schema (table row)) name)))

(defmethod (setf row-index) :after (index (row square-table-row))
  (setf (slot-value row 'row-name)
	(index-name (row-labels-schema (table row)) index)))



(defmethod table-row ((table square-table) (row-index integer) &key)
  (let ((row-name (index-name (row-labels-schema table) row-index)))
    (table-row table row-name)))

(defmethod table-row ((table square-table) (row-name t) &key)
  (make-square-table-row table row-name))

(defmethod row-ref ((row square-table-row) (col-index integer))
  (table-ref (table row) (row-index row) col-index))

(defmethod row-ref ((row square-table-row) (name t))
  (row-ref row (name-index (col-labels-schema row) name)))

(defmethod (setf row-ref) (value (row square-table-row) (col-index integer))
  (setf (table-ref (table row) (row-index row) col-index)
	value))

(defmethod (setf row-ref) (value (row square-table-row) (name t))
  (setf (row-ref row (name-index (col-labels-schema row) name))
	value))





(defclass square-table-col (table-col)
  ((table :accessor table
	  :initarg :table)
   (col-name :accessor col-name
	     :initarg :col-name)
   (col-index :accessor col-index
	      :initarg :col-index)
   (row-labels-schema :initarg :row-labels-schema
		      :accessor row-labels-schema)))


(defun make-square-table-col (table col-name)
  (make-instance 'square-table-col :table table
		 :col-name col-name
		 :col-index (name-index (col-labels-schema table) col-name)
		 :row-labels-schema (row-labels-schema table)))

(defmethod (setf col-name) :before (name (col square-table-col))
  (assert (find name (names (table col))) ()
	  (error 'non-existant-col-name :name name)))

(defmethod (setf col-name) :after (name (col square-table-col))
  (setf (slot-value col 'col-index)
	(name-index (col-labels-schema (table col)) name)))

(defmethod (setf col-index) :after (index (col square-table-col))
  (setf (slot-value col 'col-name)
	(index-name (col-labels-schema (table col)) index)))


(defmethod table-col ((table square-table) col-name &key)
  (make-square-table-col table col-name))

(defmethod (setf table-col) (values (table square-table) (col-name t))
  (setf (table-col table (name-index table col-name)) values))

(defmethod (setf table-col) ((values array) (table square-table) (col-index integer))
  (do-table-rows (row table)
    (setf (row-ref row col-index)
	  (aref values (row-index row)))))

(defmethod col-ref ((col square-table-col) (row-index integer))
  (table-ref (table col) row-index (col-index col)))

(defmethod col-ref ((col square-table-col) (name t))
  (col-ref col (name-index (row-labels-schema col) name)))

(defmethod (setf col-ref) (value (col square-table-col) (row-index integer))
  (setf (table-ref (table col) row-index (col-index col))
	value))

(defmethod (setf col-ref) (value (col square-table-col) (name t))
  (setf (col-ref col (name-index (row-labels-schema col) name))
	value))

(defmethod (setf table-row) (values (table square-table) (row-name t))
  (setf (table-row table (name-index table row-name)) values))

(defmethod (setf table-row) ((values array) (table square-table) (row-index integer))
  (do-table-cols (col table)
    (setf (col-ref col row-index)
	  (aref values (col-index col)))))


(defparameter *SES* '(:high :low :middle))
(defparameter *smoker* '(:current :former :never)
  "http://www.cyclismo.org/tutorial/R/tables.html")

(define-test square-table-accessors
  (let* ((cns (make-table-cols-schema *SES*))
	 (rns (make-table-rows-schema *smoker*))
	 (table (make-square-table rns cns :default-value 0
				   :storage 'square-table-array)))
    (assert-equal 0 (table-ref table 0 0))
    (setf (table-row table 0) #(51 43 22)
	  (table-row table 1) #(92 28 21)
	  (table-row table 2) #(68 22 9))
    (let ((tr (table-row table 0)))
      (assert-equal 51 (row-ref tr :high))
      (print (incf (row-index tr)))
      (assert-equal 28 (row-ref tr :low)))
    (let ((tc (table-col table :high)))
      (assert-equal 92 (col-ref tc :former))
      (print (incf (col-index tc) 2))
      (assert-equal 9 (col-ref tc :never)))))

