;;;; mv-table.asd

(asdf:defsystem #:mv-table
  :serial t
  :description "Describe mv-table here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lisp-unit
               #:alexandria
               #:closer-mop
	       #:antik)
  :components
  ((:module "init"
	    :serial t
	    :components
	    ((:file "table-dev-package-def")
	     #+skip(:file "mv-table-package-def")
	     #+skip(:file "column-table-package-def")
	     (:file "defaults")
	     (:file "testing-setup")
	     (:file "common-functions")
	     (:file "clos-interface")
	     (:file "table-schema")
	     (:file "flavored-table-class-instantiation")
	     (:file "utilities")
	     (:file "table-display")))
   (:module "square-table"
	    :description "Implementation of square tables that store
same-type date in the whole table"
	    :serial t
	    :components
	    ((:file "square-table-mop")
	     (:file "def-square-table")
	     (:file "def-square-table-tests")
	     (:file "square-table-schema")
	     (:file "square-table-storage")
	     (:file "square-table")
	     (:file "square-table-display")
	     (:file "square-table-iterators")
	     (:file "square-table-row+col-accessors")
	     (:file "square-table-tests")))
   #+skip(:module "mop-components"
	    :serial t
	    :components
	    ((:file "table-meta-class")
	     (:file "table-options")
	     (:file "table")
	     (:file "table-row-accessor")))
   #+skip(:module "column-table"
	    :serial t
	    :components
	    (#+skip(:file "class-initializations")
	     (:file "column-table-meta-class")
	     #+skip(:file "column-table-columns")
	     (:file "column-table-column-accessor")))
   #+skip(:module "rectangular-table")
   #+skip(:module "user-interface"
	    :serial t
	    :components
	    ((:file "user-interface")))))

