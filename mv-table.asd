;;;; mv-table.asd

(asdf:defsystem #:mv-table
  :serial t
  :description "Describe mv-table here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lisp-unit
               #:alexandria
               #:closer-mop)
  :components
  ((:module "init"
	    :serial t
	    :components
	    ((:file "table-dev-package-def")
	     #+skip(:file "mv-table-package-def")
	     (:file "column-table-package-def")
	     (:file "utilities")))
   #+skip(:module "mop-components"
	    :serial t
	    :components
	    ((:file "table-meta-class")
	     (:file "table-options")
	     (:file "table")
	     (:file "table-row-accessor")))
   (:module "column-table"
	    :serial t
	    :components
	    (#+skip(:file "class-initializations")
	     (:file "column-table-meta-class")
	     #+skip(:file "column-table-columns")
	     (:file "column-table-column-accessor")))
   #+skip(:module "matrix-table")
   #+skip(:module "rectangular-table")
   #+skip(:module "user-interface"
	    :serial t
	    :components
	    ((:file "user-interface")))))

