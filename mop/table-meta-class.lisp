(in-package :mv-table)

(defclass table-meta-class (standard-class)
  ()
  (:documentation
"Metaclass definition for mv-tables"))

(defmethod validate-superclass
            ((class table-meta-class)
	     (superclass standard-class))
   t)


(defclass column-definition
           (standard-direct-slot-definition)
   ((name :initarg :name
	  :initform (error "Must specify column name")
	  :reader column-name
	  :documentation
"Column name, a symbol")
    (vector-type :initarg :vector-type
		 :initform nil
		 :reader vector-type
		 :documentation
"Types of stored vector.  One of 'array or 'grid:foreign-array")
    (element-type :initarg :element-type
		  :initform (error "Must specify element type")
		  :reader column-element-type
		  :documentation
"Element type stored in this column")
    (comparator :initarg :comparator
		:initform nil
		:reader column-comparator
		:documentation
"Function of two arguments that compares for sorting purposes")
    (equality-predicate :initarg :equality-predicate
			:initform nil
			:reader column-equality-predicate
			:documentation
"Function of two arguments that tests for their equality")
    (default-value :initarg :default-value
      :initform (error "Must specify default value")
      :reader column-default-value
      :documentation
"Default value for column")
    (unspecified-value :initarg :unspecified-value
		       :initform (error "Must specify unspecified value")
		       :reader column-unspecified-value
		       :documentation
"Value that signals that this column element has not been specified")
    (value-normalizer :initarg :value-normalizer
		      :initform nil
		      :reader value-normalizer
		      :documentation
"Function of two arguments: an input value, and the object of the
column where the value is going.

This function is used to normalize an input value to an appropriate
type"))
    (:documentation "Class for specifying column properties"))

(defmethod column-definition-class
    ((class table-meta-class) &key &allow-other-keys)
  (find-class 'column-definition))

#+skip(defclass column-type-effective-slot-definition
    (standard-effective-slot-definition)
  ((vector-type :initarg :vector-type :initform nil
		:accessor vector-type)))

(defmethod effective-slot-definition-class
    ((class table-meta-class) &key &allow-other-keys)
  (find-class 'column-definition))

(defmethod compute-effective-slot-definition-initargs
    ((class table-meta-class) direct-slots)
  (let* ((attributes nil))
    (dolist (slot direct-slots)
      (when slot
        (unless vector-type
          (setq vector-type (vector-type slot)))))
    (append (call-next-method) (list :vector-type vector-type))))

(defmethod compute-slots ((class table-meta-class))
  (let* ((normal-slots (call-next-method))
         (alist
          (mapcar
           #'(lambda (slot)
               (cons (slot-definition-name slot)
                     (mapcar #'(lambda (attr) (cons attr nil))
                             (vector-type slot))))
           normal-slots)))
    (cons (make-instance 'standard-effective-slot-definition
                         :name 'all-attributes
                         :initform `',alist
                         :initfunction #'(lambda () alist))
          normal-slots)))

(defun vector-type (instance slot-name vector-type)
  (cdr (vector-type-bucket instance slot-name vector-type)))

(defun (setf vector-type) (new-value instance slot-name vector-type)
  (setf (cdr (vector-type-bucket instance slot-name vector-type))
        new-value))


(defun vector-type-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
         (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~S of ~S has no attributes."
             slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
        (error "The slot named ~S of ~S has no attribute named ~S."
             slot-name instance attribute))
      attr-bucket)))




#|
;;; https://groups.google.com/d/msg/comp.lang.lisp/oxjqB21hRB8/uWWu_0_A2OMJ
(in-package :cl-user)
(use-package :sb-mop)

(defclass attributes-class (standard-class) ())

(defmethod sb-pcl:validate-superclass
    ((class attributes-class) (superclass sb-mop:standard-class))
  t)

(defclass attribute-direct-slot-definition
    (sb-pcl:standard-direct-slot-definition)

  ((attributes :initarg :attributes :initform nil
               :accessor slot-definition-attributes)))

(defmethod sb-pcl:direct-slot-definition-class
    ((class attributes-class) &key &allow-other-keys)
  (find-class 'attribute-direct-slot-definition))

(defclass attribute-effective-slot-definition
    (sb-pcl:standard-effective-slot-definition)

  ((attributes :initarg :attributes :initform nil
               :accessor slot-definition-attributes)))

(defmethod sb-pcl:effective-slot-definition-class
    ((class attributes-class) &key &allow-other-keys)
  (find-class 'attribute-effective-slot-definition))

(defmethod sb-pcl::compute-effective-slot-definition-initargs
    ((class attributes-class) direct-slotds)
  (let* ((attributes nil))
    (dolist (slotd direct-slotds)
      (when slotd
        (unless attributes
          (setq attributes (slot-definition-attributes slotd)))))
    (append (call-next-method) (list :attributes attributes))))

(defmethod sb-pcl:compute-slots ((class attributes-class))

  (let* ((normal-slots (call-next-method))
         (alist
          (mapcar
           #'(lambda (slot)
               (cons (sb-pcl:slot-definition-name slot)

                     (mapcar #'(lambda (attr) (cons attr nil))
                             (slot-definition-attributes slot))))
           normal-slots)))
    (cons (make-instance 'sb-pcl:standard-effective-slot-definition

                         :name 'all-attributes
                         :initform `',alist
                         :initfunction #'(lambda () alist))
          normal-slots)))

(defun slot-attribute (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))

(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket instance slot-name attribute))
        new-value))

(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
         (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~S of ~S has no attributes."
             slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
        (error "The slot named ~S of ~S has no attribute named ~S."
             slot-name instance attribute))
      attr-bucket)))

(defclass credit-rating ()
  ((level :initform 1 :initarg :level :attributes (date-set time-set)))
  (:metaclass attributes-class))
|#
