(in-package :table-dev)

(defvar *table-selector* nil)

(defun add-table-class (format direction device protocol class)
  (setf *table-selector*
	(acons (list format direction device protocol) class
	       *table-selector*)))

(defun compare-table-lists (list1 list2)
  (every #'eql list1 list2))

(defun select-table-class (format direction device protocol)
  (let ((entry (assoc (list format direction device protocol)
		       *table-selector*
		       :test #'compare-table-lists)))
    (assert entry ()
	    (error 'request-class-undefined
	       :format format
	       :direction direction
	       :device device
	       :protocol protocol))
	 (cdr entry)))



;;; From MOP

(defun check-flavor-types (format direction
			    device protocol)
  (assert (every #'subtypep (list format direction
				  device protocol)
		 '(table-format table-direction table-device table-protocol))
	  ()
	  (error 'incorrect-table-flavor
		 :format format
		 :direction direction
		 :device device
		 :protocol protocol)))

(defun make-programmatic-table-instance (format direction
				   device protocol &rest initargs)
  (let ((superclass-names (list format direction device protocol)))
    (apply #'check-flavor-types superclass-names)
    (apply #'make-instance
	   (apply #'find-programmatic-table-class superclass-names)
	   initargs)))




(defun find-programmatic-table-class (format direction
				      device protocol)
  (let ((superclass-names (list format direction device protocol)))
    (let* ((superclasses (mapcar #'find-class superclass-names))
	   (format-subclasses
	    (class-direct-subclasses (car superclasses))))
      (if format-subclasses
	  (let ((class (find-if
			#'(lambda (class)
			    (equal superclasses
				   (class-direct-superclasses class)))
			format-subclasses)))
	    (if class
		class
		(make-programmatic-table-class superclass-names
		       superclasses)))
	  (make-programmatic-table-class superclass-names
		 superclasses)))))

(defun make-programmatic-table-class (name super-classes)
  "Create an instance of a standard class.  NAME is the class name,
SUPER-CLASSES a list of the class direct supers (as classes, not
names)"
  (make-instance 'standard-class
		 :name name
		 :direct-superclasses super-classes
		 :direct-slots ()))


(define-test make-programmatic-instance
  (let* ((superclass-names (list 'test-format 'test-direction
				 'test-device 'test-protocol))
	 (superclasses (mapcar #'find-class superclass-names))
	 (object (apply #'make-programmatic-table-instance superclass-names))
	 (class (class-of object))
	 (name (class-name class)))
    (assert-true (subtypep (type-of object)
			   'test-format))
    (assert-true (subtypep (type-of object)
			   'test-direction))
    (assert-true (subtypep (type-of object)
			   'test-device))
    (assert-true (subtypep (type-of object)
			   'test-protocol))
    (assert-equal '(test-format test-direction
		    test-device test-protocol) name)))
