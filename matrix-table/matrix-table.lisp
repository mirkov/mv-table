(in-package :mv-table)

(defmacro def-matrix-table (name direct-supertables direct-columns
			    &rest table-options)
  "Define a column table"
  `(ensure-class ,name
		 :direct-supertables ,(canonicalize-direct-supertables
				       direct-supertables)
		 :direct-column-definitions
		 ,(canonicalize-direct-columns direct-columns)
		 ,@(canonicalize-deftable-options table-options)
		 :metaclass 'column-table))


(defclass matrix-table (standard-class)
  ()
  (:documentation "Meta-class for matrix tables"))
(defmethod validate-superclass ((self matrix-table) (super standard-class))
  t)

;;; Unresolved - how do I specify sorting and comparing matrix rows?

(defclass matrix-table-direct-slot-definition (standard-direct-slot-definition
					       column-table-direct-slot-definition)
  ((data-type
    :documentation "Type of data stored in the matrix table.  This is a CL type")
   (access
    :documentation "Random or sequential access type")
   (organization
    :documentation ":file or :memory or :tunnel")
   (action
    :documentation ":read, :write, or :read+write")
   (vector-type
    :documentation "How is the matrix stored in the lisp image?  One
   of :array or :foreign-array")
   (value-normalizer
    :documentation "A function of two arguments (value, matrix schema
    -what exactly do I mean by that?)  that normalizes the value prior
    to storing into matrix. ")
   (null-value
    :documentation "Value that signifies un-assigned matrix value")
   (equality-predicate
    :documentation "A function of two arguments")
   (comparator
    :documentation "A function of two arguments")
   ))
