(in-package :mv-table)


(defmacro deftable (name direct-supertables direct-columns &rest table-options)
  "Canonicalize deftable arguments and pass them to `ensure-table'"
  ;; modeled after MOP defclass, p.285
  ;;
  ;; here we start the descent into MOP - by specifying the metaclass
  ;; as mv-table
  `(apply #'ensure-class ,name
	  :direct-slots ,(canonicalize-direct-columns direct-columns)
	  :direct-superclasses ,(canonicalize-direct-supertables
				 direct-supertables)
	  :metaclass 'mv-table
	  ,@(canonicalize-deftable-options table-options)))
