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
	    ((:file "mv-table-package-def")))
   (:module "mop"
	    :serial t
	    :components
	    ((:file "table-meta-class")
	     (:file "columns")
	     (:file "table")
	     (:file "table-row-accessor")))))

