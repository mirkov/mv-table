(in-package :table-dev)

(progn 
  (def-square-table test-table ()
    ((row-labels-schema :names (a b c)
			:comparator #'char<)
     (col-labels-schema :names (1 2 3)
			:equality-predicate #'=
			:value-normalizer #'floor))
    (:documentation "Test table for unit tests")
    (:value-normalizer #'floor)))

(define-test def-square-table
  "Test of the class stracture"
  (let* ((tt-class (find-class 'test-table))
	 (tt-meta-class (class-of tt-class))
	 (meta-class-name (class-name tt-meta-class)))
    (assert-equal 'square-table-class meta-class-name)
    (assert-equal '(row-labels-schema col-labels-schema)
		  (mapcar #'slot-definition-name (class-direct-slots tt-class)))
    (assert-equal '(schema-direct-slot-definition schema-direct-slot-definition)
		  (mapcar (compose #'class-name #'class-of)
			  (class-direct-slots tt-class)))
    (assert-equal '(schema-direct-slot-definition schema-direct-slot-definition)
		  (mapcar (compose #'class-name #'class-of)
			  (class-direct-slots tt-class)))
    (remove-if (lambda (package-name)
		 (string= package-name "CCL"))
	       (mapcar #'slot-definition-name (class-slots tt-meta-class))
	       :key (compose #'package-name #'symbol-package))
    (slot-boundp tt-meta-class 'value-normalizer)))
