(in-package :table-dev)

(progn 
  (def-square-table test-table ()
    ((row-labels-schema :names (a b c)
			:equality-predicate string=
			:comparator char<)
     (col-labels-schema :names (1 2 3)
			:equality-predicate equal
			:value-normalizer floor))
    (:documentation "Test table for unit tests")
    (:value-normalizer #'floor)))

(defmethod initialize-instance :after ((self test-table) &key &allow-other-keys)
  )

(defun tt-slot-names ()
  (let* ((tt-class (find-class 'test-table))
	 (tt-meta-class (class-of tt-class))
	 (meta-class-name (class-name tt-meta-class)))
    (mapcar #'slot-definition-name (class-direct-slots tt-class))))

(define-test def-square-table
  "Test of the class structure"
  (let* ((tt-class (find-class 'test-table))
	 (tt-meta-class (class-of tt-class))
	 (meta-class-name (class-name tt-meta-class)))
    (assert-equal 'square-table-class meta-class-name)
    (assert-equal '(row-labels-schema col-labels-schema value-normalizer)
		  (mapcar #'slot-definition-name (class-direct-slots tt-class)))
    (assert-equal '(schema-direct-slot-definition schema-direct-slot-definition
		    standard-direct-slot-definition)
		  (mapcar (compose #'class-name #'class-of)
			  (class-direct-slots tt-class)))
    (remove-if (lambda (package-name)
		 (string= package-name "CCL"))
	       (mapcar #'slot-definition-name (class-slots tt-meta-class))
	       :key (compose #'package-name #'symbol-package))
    (slot-boundp tt-class 'value-normalizer)))


(progn
  (let ((table (make-instance 'test-table)))
    (assert-equal 3   (funcall (value-normalizer table) pi))
    #+skip(slot-value table 'row-labels-schema)))
