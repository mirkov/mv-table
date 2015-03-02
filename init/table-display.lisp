(in-package :table-dev)

(defclass table-display ()
  ((value-normalizer :initarg :value-normalizer
		     :accessor value-normalizer))
  (:documentation "Stores information regarding the processing of table data 
for display")
  (:default-initargs :value-normalizer (lambda (v)
					 v)))

(defgeneric display-table (stream format table)
  (:documentation "Display TABLE using FORMAT on STREAM"))


(defclass table-format () ()
  (:documentation "Stores information that controls how the values
are presented"))

(defclass tabular-format (table-format)
  ((column-width :initarg :column-width
		 :accessor column-width))
  (:documentation "Stores information that controls the tabular layout
 of table values"))



(defclass csv-format (table-format)
  ((separator :initarg :separator
	      :accessor separator))
  (:default-initargs :separator #\,)
  (:documentation "Specifies how the tables are displayed in CSV
  format"))


