(in-package :table-dev)

(defclass table-rows-schema (1d-schema)
  ((count :accessor num-rows))
  (:documentation "Schema for storing, accessing, comparing row labels"))

(defun make-table-rows-schema (names &rest keys
			       &key equality-predicate value-normalizer
				 #|comparator|#)
  (declare (ignore equality-predicate value-normalizer
		   #|comparator|#))
  (apply #'make-instance 'table-rows-schema :names names keys))


(defclass table-cols-schema (1d-schema)
  ((count :accessor num-cols))
  (:documentation "Schema for storing, accessing, comparing column labels"))

(defun make-table-cols-schema (names &rest keys
			       &key  equality-predicate value-normalizer
				 #|comparator|#)
  (declare (ignore equality-predicate value-normalizer
		   #|comparator|#))
  (apply #'make-instance 'table-cols-schema :names names keys))

(define-test table-rows-schema
  (let ((s (make-table-rows-schema *people-names*)))
    (assert-equal 4 (slot-value s 'count)))
  (let ((s1 (make-table-rows-schema  '("a" "b" "c")
			     :equality-predicate #'string=)))
    (describe s1)
    (assert-equal 1 (name-index s1 "b"))
    (assert-true (not (name-index s1 "d")))))

(define-test table-cols-schema
  (let ((s (make-table-cols-schema *people-names*)))
    (assert-equal 4 (slot-value s 'count)))
  (let ((s1 (make-table-cols-schema  '("a" "b" "c")
			     :equality-predicate #'string=)))
    (describe s1)
    (assert-equal 1 (name-index s1 "b"))
    (assert-true (not (name-index s1 "d")))))
