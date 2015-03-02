(in-package :table-dev)


(define-test arr-table
  (let* ((rls (make-row-labels-schema :names *employees*
				      :equality-predicate 'eql))
	 (cls (make-col-labels-schema :names *days-of-week*
				      :equality-predicate 'eql))
	 (table (make-square-table rls cls
				   :default-value 3.14
				   :storage 'square-table-array)))
    table
    (assert-equal 3.14 (table-ref table 'koichi 'tue))
    (setf (table-ref table 'koichi 'tue) 2.17)
    (assert-equal 2.17 (table-ref table 'koichi 'tue))
    (assert-equal '(7 4) (array-dimensions (2d-structure (data-matrix table))))
    (assert-equal '(7 4) (table-dimensions table)))
  (let* ((rls (make-row-labels-schema :names *employees*
				      :equality-predicate 'eql))
	 (cls (make-col-labels-schema :names *days-of-week*
				      :equality-predicate 'eql))
	 (table (make-square-table rls cls
				   :default-value 3.14
				   :storage 'square-table-hash^2)))
    (assert-equal 3.14 (table-ref table 'koichi 'tue))
    (setf (table-ref table 'koichi 'tue) 2.17)
    (assert-equal 2.17 (table-ref table 'koichi 'tue))
    (assert-equal '(7 4) (table-dimensions table))))


(define-test adjustable-table
  (let* ((cns (make-table-cols-schema nil))
	 (rns (make-table-rows-schema nil))
	 (table (make-square-table rns cns :default-value 0
				   :storage 'square-table-hash^2
				   :adjustable t)))
    (setf (table-ref table :current :high) 51
	  (table-ref table :former :low) 28
	  (table-ref table :never :middle) 9)
    (assert-equalp #(:current :former :never)
		   (names (row-labels-schema table)))
    (assert-equalp #(:high :low :middle)
		   (names (col-labels-schema table)))
    (assert-equal 51 (table-ref table :current :high))
    (assert-equal 28 (table-ref table :former :low))
    (assert-equal 9 (table-ref table :never :middle))
    (let ((fmt (make-square-table-tabular-format 10 10))
	  (str (make-array '(0) :element-type 'base-char
			   :fill-pointer 0 :adjustable t)))
      (with-output-to-string (stream str)
	(display-table stream fmt table))
      (assert-equalp 
       "          HIGH      LOW       MIDDLE
CURRENT   51        0         0
FORMER    0         28        0
NEVER     0         0         9
" str))
    (let ((fmt (make-square-table-csv-format))
	  (str (make-array '(0) :element-type 'base-char
			   :fill-pointer 0 :adjustable t)))
      (with-output-to-string (stream str)
	(display-table stream fmt table))
      (assert-equalp 
       ",HIGH,LOW,MIDDLE
CURRENT,51,0,0
FORMER,0,28,0
NEVER,0,0,9
" str))))
