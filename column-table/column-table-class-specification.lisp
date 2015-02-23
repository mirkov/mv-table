(in-package :mv-table)

(defmacro def-column-table (name direct-supertables direct-columns
			    &rest table-options)
  "Define a column table"
  `(ensure-class ,name
		 :direct-superclasses ,(canonicalize-direct-supertables
					direct-supertables)
		 :direct-slots ,(canonicalize-direct-columns direct-columns)
		 ,@(canonicalize-deftable-options table-options)
		 :metaclass 'column-table))







