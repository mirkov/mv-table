(in-package :mv-table)



;; Redefining metaobject class #<STANDARD-CLASS TABLE-META-DATA :VERSION 2> has no effect.

(defclass table-meta-data-mixin ()
  ((source :initarg :source
	   :reader table-source
	   :documentation
	   "Documentation of the source of the table's data
- A string describing the source of the table,
- A pathname to a file from which the table was imported.

Loaded tables retain their original  source")
   (build-method :initarg :build-method
		 :accessor table-build-method
		 :documentation
		 "Keyword specifying the table build method:
- nil
- :column-wise
- :row-wise")
   (state :reader table-state
	  :documentation
	  "Keyword specifying the table's current state
- nil 
- :complete -- Table has been completely filled according to table size and row-count
               and column-count
- :incomplete -- in case not all rows or columns have been specified
- :adding-rows -- The table is in the process of adding rows")
   (row-count :reader table-row-count
	      :initarg :row-count
	      :documentation
	      "Table row count:
- current row count if build-method is :row-wise
- specified row count if build-method is :column-wise")
   (column-count :reader table-column-count
		 :documentation
		 "Table column count:
- Specified when the table is defined using deftable"))
  (:documentation "Stores table meta data.

Some of the data is mutable, and other (with reader methods)
unmutable"))


(defun table-meta-class-p (class-or-name)
  "If CLASS-OR-NAME is a table meta class, return the meta-class.
Otherwise return nil"
  (let ((class (class-of (if (symbolp class-or-name)
			     (find-class class-or-name)
			     class-or-name))))
    (if (equal 'table-meta-class (class-name class))
	class nil)))

(defun find-table (table-name)
  (let* ((class (class-of (find-class table-name)))
	 (class-name (class-name class)))
    (assert (equal 'table-meta-class class-name) ()
	    (error "table-name, ~a, which is a ~a class must be a TABLE-META-CLASS"
			  table-name class-name))
    class))

(defun canonicalize-direct-supertable (table-name)
  `(find-table ',table-name))

(defun canonicalize-direct-supertables (direct-supertables)
  `(list ,@(mapcar #'canonicalize-direct-supertable direct-supertables)
	 (find-class 'table-meta-data-mixin)))

(defun canonicalize-deftable-option (option)
  (case (car option)
    (:documentation (list ':documentation (cadr option)))))

(defun canonicalize-deftable-options (options)
  #-inheritence-supported
  (when options (error "table inheritance is not supported yet"))
  #+inheritence-supported (mappend #'canonicalize-deftable-option options))

(defmacro deftable (name direct-supertables
		    column-definitions
		    &rest table-options)
  `(ensure-class ',name
		 :direct-superclasses ,(canonicalize-direct-supertables
					direct-supertables)
		 :metaclass 'table-meta-class
		 ,@(canonicalize-deftable-options table-options)))


(define-test deftable-expansion
  ""
  (assert-expands
   ()
   (deftable test-table1 () ()))
  (assert-expands
   ()
   (deftable test-table2 (test-table1) ()))
  (assert-expands
   ()
   (deftable test-table3 (test-table2) ()
	     (:documentation "test table 3"))))


(define-test deftable-definition
  "tests that the tables are defined correctly"
  (let* ((tt (deftable test-table1 () ()))
	 (supers (mapcar #'class-name (class-direct-superclasses tt))))
    (assert-true tt)
    (assert-true
     (notany #'null
	     (mapcar (lambda (test-super)
		       (find test-super supers)) '(table-meta-data-mixin)))))
  (let* ((tt (deftable test-table2 (test-table1) ()))
	 (supers (mapcar #'class-name (class-direct-superclasses tt))))
    (assert-true tt)
    (assert-true
     (notany #'null
	     (mapcar (lambda (test-super)
		       (find test-super supers)) '(table-meta-data-mixin test-table1)))))

  (let* ((tt (deftable test-table3 (test-table2) ()
	     (:documentation "test table 3")))
	(supers (mapcar #'class-name (class-direct-superclasses tt)))
	(doc (documentation 'test-table3 'type)))))



(define-test deftable-validity)


(defun make-table (name &key
			  source build-method row-count)
  (let (kwd-arguments)
    (when source (setf (getf kwd-arguments :source) source))
    (when build-method (setf (getf kwd-arguments :build-method) build-method))
    (when row-count (setf (getf kwd-arguments :row-count) row-count))
    (apply #'make-instance name kwd-arguments)))


(define-test make-table
  "Test of table definition and instantiation"
  (let ((table (deftable )))))







