(in-package :table-dev)

(defclass square-table-display (table-display)
  ((row-names-normalizer :initarg :row-names-normalizer
			 :accessor row-names-normalizer)
   (column-names-normalizer :initarg :column-names-normalizer
			    :accessor column-names-normalizer))
  (:documentation "Adds definition of row and column names")
  (:default-initargs
      :row-names-normalizer (lambda (v)
			      v)
    :column-names-normalizer (lambda (v)
			       v)))

(defclass square-table-tabular-format (tabular-format square-table-display)
  ((row-labels-width :initarg :row-labels-width
		     :accessor row-labels-width)
   (col-width :initarg :col-width
	      :accessor col-width)))

(defun make-square-table-tabular-format
    (row-labels-width column-width
     &rest keys &key
		  value-normalizer
		  row-names-normalizer
		  column-names-normalizer)
  (declare (ignore value-normalizer row-names-normalizer column-names-normalizer))
  (apply #'make-instance 'square-table-tabular-format
	 :row-labels-width row-labels-width
	 :column-width column-width
	 keys))


(defclass square-table-csv-format (csv-format square-table-display)
  ())

(defun make-square-table-csv-format (&rest keys
				     &key separator
				       value-normalizer
				       row-names-normalizer
				       column-names-normalizer
				       &allow-other-keys)
  "Make CSV format.  This function accepts a single argument that
should be of character type"
  (declare (ignore separator value-normalizer row-names-normalizer
		   column-names-normalizer))
  (apply #'make-instance 'square-table-csv-format keys))

(defmethod display-table (stream (format square-table-tabular-format)
			  (table square-table))
  (let ((row-labels-width (row-labels-width format))
	(cols-width (column-width format))
	(value-normalizer (value-normalizer format))
	(row-names-normalizer (row-names-normalizer format))
	(col-names-normalizer (column-names-normalizer format)))
    (let ((col-names (names (col-labels-schema table))))
      (loop :with col-offset = row-labels-width
	 :for name :across col-names
	 :do (format stream "~vt~a" col-offset
		     (funcall col-names-normalizer name))
	 :do (incf col-offset cols-width))
      (format stream "~%")
      (let ((row-names (names (row-labels-schema table))))
	(loop :for row-name :across row-names :do
	   (progn
	     (format stream "~a" (funcall row-names-normalizer row-name))
	     (loop :with col-offset = row-labels-width
		:for col-name :across col-names
		:for value = (table-ref table row-name col-name)
		:do (format stream "~vt~a" col-offset (funcall value-normalizer value))
		:do (incf col-offset cols-width))
	     (format stream "~%")))))))



(defmethod display-table (stream (format csv-format) (table square-table))
  (let ((separator (separator format))
	(value-normalizer (value-normalizer format))
	(row-names-normalizer (row-names-normalizer format))
	(col-names-normalizer (column-names-normalizer format)))
    (let ((col-names (names (col-labels-schema table))))
      (loop :for name :across col-names
	   :do (format stream "~a~a" separator
		       (funcall col-names-normalizer name)))
      (format stream "~%")
      (let ((row-names (names (row-labels-schema table))))
	;;(break)
	(loop :for row-name :across row-names :do
	  (progn
	    (format stream "~a" (funcall row-names-normalizer row-name))
	    (loop :for col-name :across col-names
	       :for value = (table-ref table row-name col-name)
	       :do (format stream "~a~a" separator (funcall value-normalizer value)))
	    (format stream "~%")))))))
