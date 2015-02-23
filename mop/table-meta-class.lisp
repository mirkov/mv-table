(in-package :mv-table)

(defun canonicalize-direct-supertables (&rest supertables)
  ;; Modeled after tAotMOP, p. 286-287
  #-table-inheritance
  (when supertables
    (error "Table inheritance is not yet enabled")))


(defclass table-meta-class (standard-class)
  ()
  (:documentation
"Metaclass definition for mv-tables"))

(defmethod validate-superclass
            ((class table-meta-class)
	     (superclass standard-class))
   t)







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




