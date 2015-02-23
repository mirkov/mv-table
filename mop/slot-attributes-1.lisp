(defpackage :attribute-class
  (:use #:closer-common-lisp)
  (:documentation "Extends CLOS slots to accept attributes

Attributes are locations, attached to slots, which can be used to
store information about the value in the slot

Discussed in The Art of the MetaObject Protocol, Section 3.5

This code is the MOP implementation of the code in the book

Code was posted on comp.lang.lisp:
https://groups.google.com/d/msg/comp.lang.lisp/oxjqB21hRB8/uWWu_0_A2OMJ"))



(in-package :attribute-class)


(defclass attributes-class (standard-class) ())

(defmethod validate-superclass
    ((class attributes-class) (superclass standard-class))
  t)

(defclass attributes-slot-mixin ()
  ((attributes :initarg :attributes :initform nil
               :accessor slot-definition-attributes))
  (:documentation "Contains attribute slot definition"))

(defclass attribute-direct-slot-definition
    (standard-direct-slot-definition attributes-slot-mixin)
  ;; The direct slot definition class is augmented with the attributes
  ;; slot
  ()
  (:documentation "direct slot definition class that uses the mixin to
  load the slot definition"))

(defmethod direct-slot-definition-class
    ((class attributes-class) &key &allow-other-keys)
  "Return attribute-direct-slot-definition-class instead of
standard-direct-slot-definition"
  (find-class 'attribute-direct-slot-definition))

(defclass attribute-effective-slot-definition
    (standard-effective-slot-definition attributes-slot-mixin)
  ;; The effective slot definition class is augmented with the
  ;; attribute slot
  ()
  (:documentation "effective slot definition class that uses the mixin
  to load the slot definition"))

(defmethod effective-slot-definition-class
    ((class attributes-class) &key &allow-other-keys)
  "Return attribute-effective-slot-definition-class instead of
standard-effective-slot-definition"
  (print 2)
  (find-class 'attribute-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class attributes-class) name direct-slotds)
  "Set effective-slot-definition attribute slot to the value in the
direct-slot-definition's attribute-slot

We handle inheritance by collecting all attributes in order of "
  (let* ((attributes-raw
	  ;; first collect all attributes in decreasing class
	  ;; specificity, and then nreverse the list
	  (let ((storage nil))
	    (dolist (slotd direct-slotds (nreverse storage))
	      ;; not sure why this is here - can slotd be nil?
	      (when slotd
		(setf storage
		      (append (slot-definition-attributes slotd) storage))))))
	 (attributes
	  ;; clean up by removing nil's and duplicates.  We remove the
	  ;; duplicates from the end, to keep the inheritance order
	  (remove-duplicates
	   (remove-if #'null attributes-raw) :from-end t))
	 (effective-slot-definition (call-next-method)))
    ;; set the effective-slot-definitions attribute slot to the
    ;; attributes list
    (setf (slot-definition-attributes effective-slot-definition)
	  attributes)
    ;; and return the effective slot definition
    effective-slot-definition))



(defmethod compute-slots ((class attributes-class))
  "The attributes from each slot are collected into an alist, which is
stored in the all-attributes slot that is appended to the rest of the
class slots

Slot attributes thus are not stored in the slot themselves.  The are
set and retreived by looking up their place in the all-attributes
slot

This slot is visible via `slot-value'.

Note that all-attributes is a standard-slot, not an attribute slot

To access the slot-attributes, we define accessor functions below."
  (let ((normal-slots (call-next-method)))
    (let ((alist
	   (mapcar
	    #'(lambda (slot)
		(print slot)
		(cons (print (slot-definition-name slot))
		      (mapcar #'(lambda (attr) (cons attr nil))
			      (slot-definition-attributes slot))))
	    normal-slots)))
      (cons (make-instance 'standard-effective-slot-definition
			       :name 'all-attributes
			       :initform `',alist
			       :initfunction #'(lambda () alist))
	    normal-slots))))

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

(defclass credit-rating-1 (credit-rating)
  ((level :attributes (assistant)))
  (:metaclass attributes-class))

#|
(defparameter *credit-rating* (make-instance 'credit-rating))
(slot-value *credit-rating* 'all-attributes)
(slot-attribute *credit-rating* 'level 'date-set)
(setf (slot-attribute *credit-rating* 'level 'date-set) "12/15/90")
(defparameter *credit-rating-1* (make-instance 'credit-rating-1))
(slot-value *credit-rating-1* 'all-attributes)
|#
