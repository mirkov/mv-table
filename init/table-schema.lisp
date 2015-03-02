(in-package :table-dev)

(defmethod add-name ((self 1d-schema) name)
  "Add name to the schema, linking an index to it.  Also increment COUNT

Return values INDEX and NAME"
  (with-slots (names count) self
    (vector-push-extend name names)
    (incf count)
    (values (- count 1) name)))

(defmethod initialize-instance :after ((self 1d-schema)
				       &key names)
  (dolist (name names)
    (add-name self name)))

(defun make-1d-schema (names &rest keys &key value-normalizer equality-predicate
				 #|comparator|#)
  (declare (ignore value-normalizer equality-predicate #|comparator|#))
  (apply #'make-instance '1d-schema :names names keys))

(defmethod print-object ((self 1d-schema) s)
  (print-unreadable-object (self s :type t :identity t)
    (format s "~a names" (slot-value self 'count))))

(defmethod describe-object ((self 1d-schema) s)
  (with-slots (count names) self
    (format s "~a has ~a names~%" self count)
    (format s "They are:~%")
    #+skip(loop :for index :being :the :hash-keys :in (slot-value self 'names)
	     :using (hash-value name)
	     :do (format s "~a: ~a~%" index name))
    (loop :for index :below count
       :do (format s "~a: ~a~%" index (aref names index)))))

(defgeneric name-index (schema name)
  (:documentation "Return the index corresponding to name.  Return NIL
  if name is not found in schema"))

(defmethod name-index ((self 1d-schema) (name t))
  (position name (slot-value self 'names)
	    :test (slot-value self 'equality-predicate)))

(defgeneric index-name (schema index)
  (:documentation "Return the name corresponding to index.  Return NIL
  if name is not found in schema"))

(defmethod index-name ((self 1d-schema) (index integer))
  (aref (slot-value self 'names) index))

(define-test 1d-schema
  (let ((s (make-1d-schema *people-names*)))
    (assert-equal 4 (slot-value s 'count)))
  (let ((s1 (make-1d-schema  '("a" "b" "c")
			     :equality-predicate #'string=)))
    (describe s1)
    (assert-equal 1 (name-index s1 "b"))
    (assert-true (not (name-index s1 "d")))))
