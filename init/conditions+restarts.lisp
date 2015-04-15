(in-package :table-dev)

(define-condition undefined-table-type (error)
  ((table-type :initarg :table-type
	:reader table-type))
  (:report (lambda (c s)
	     (format s "Cannot instantiate table of type ~a because it is undefined"
		     (table-type c))))
  (:documentation "For handling errors when trying to deal with
  undefined table type"))

(define-condition unspecified-device (error)
  ()
  (:report (lambda (c s)
	     (declare (ignore c))
	     (format s "Table device has not been defined in DEFCLASS")))
  (:documentation "Error thrown when we try to define a table, but not
  specify the device.  Device is specified by the class itself"))

(define-condition unspecified-direction (error)
  ()
  (:report (lambda (c s)
	     (declare (ignore c))
	     (format s "Table direction has not been defined in DEFCLASS")))
  (:documentation "Error thrown when we try to define a table, but not
  specify the direction.  Direction is specified by the class itself"))

(define-condition unspecified-format (error)
  ()
  (:report (lambda (c s)
	     (declare (ignore c))
	     (format s "Table format has not been defined in DEFCLASS")))
  (:documentation "Error thrown when we try to define a table, but not
  specify the format.  Format is specified by the class itself"))

(define-condition unspecified-value-type (error)
  ()
  (:report (lambda (c s)
	     (declare (ignore c))
	     (format s "Must specify table value type")))
  (:documentation "Error is thrown when we must have the value type
  specified"))

(define-condition table-flavors (error)
  ((format :initarg :format
	   :reader table-format)
   (direction :initarg :direction
	      :reader direction)
   (device :initarg :device
	   :reader device)
   (protocol :initarg :protocol
	     :reader protocol))
  (:documentation "Parent condition for conditions dealing with table
  flavors"))

(define-condition request-class-undefined (table-flavors)
  ()
  (:report (lambda (c s)
	     (format s "Table class is not defined ~%")
	     (format s "Format: ~a~%" (table-format c))
	     (format s "Direction: ~a~%" (direction c))
	     (format s "Device: ~a~%" (device c))
	     (format s "Protocol: ~a~%" (protocol c)))))

(define-condition incorect-table-flavor (table-flavors)
  ()
  (:report (lambda (c s)
	     (format s "Some of table flavor specification are of incorrect type~%")
	     (format s "Format flavor is of type ~a, but should be TABLE-FORMAT"
		     (table-format c))
	     (format s "Direction flavor is of type ~a, but should be TABLE-DIRECTION"
		     (direction c))
	     (format s "Device flavor is of type ~a, but should be TABLE-DEVICE"
		     (device c))
	     (format s "Protocol flavor is of type ~a, but should be TABLE-PROTOCOL"
		     (protocol c)))))


(define-condition table-option-error (error)
  ((name :initarg :option-name
	 :reader option-name))
  (:documentation "Base condition for errors stemming from invalid
  table option definition"))

(define-condition table-option-not-function (table-option-error)
  ()
  (:report (lambda (c s)
	     (format s "Table option ~a value is not a function" (option-name c)))))

(define-condition illegal-table-option (table-option-error)
  ()
  (:report (lambda (c s)
	     (format s "Table option ~a is not allowed" (option-name c)))))

(define-condition duplicate-table-option (table-option-error)
  (#+skip(:table-options :initarg :table-options
		  :reader table-options))
  (:report (lambda (c s)
	     (format s "Table option ~a is a duplicate" (option-name c)))))

(define-condition table-option-missing-value (table-option-error)
  ()
  (:report (lambda (c s)
	     (format s "Table option ~a is missing a value"
		     (option-name c)))))

(define-condition illegal-table-name (error)
  ((name :initarg :table-name
	 :reader table-name))
  (:report (lambda (c s)
	     (format s "~a is an invalid table name" (table-name c)))))

(define-condition illegal-table-slot (error)
  ((name :initarg :slot-name
	 :reader slot-name)))

(define-condition illegal-table-slot-name (illegal-table-slot)
  ()
  (:report (lambda (c s)
	     (format s "~a is an invalid table slot name" (slot-name c)))))

(define-condition duplicate-table-slot (illegal-table-slot)
  ()
  (:report (lambda (c s)
	     (format s "~a is a duplicate table slot" (slot-name c)))))

(define-condition illegal-table-slot-option (illegal-table-slot)
  ((option-name :initarg :slot-option-name
		:reader slot-option-name)))

(define-condition missing-table-slot-option (illegal-table-slot-option)
  ()
  (:report (lambda (c s)
	     (format s "Missing option ~a in slot ~a"
		     (slot-option-name c) (slot-name c)))))

(define-condition duplicate-table-slot-option (illegal-table-slot-option)
  ()
  (:report (lambda (c s)
	     (format s "Duplicate option ~a for slot ~a"
		     (slot-option-name c)
		     (slot-name c)))))

(define-condition illegal-table-slot-option-value (illegal-table-slot-option)
  ((option-value :initarg :slot-option-value
		 :reader slot-option-value))
  (:report (lambda (c s)
	     (format s "Slot ~a option ~a value ~a is illegal"
		     (slot-name c) (slot-option-name c)
		     (slot-option-value c)))))

(define-condition missing-table-slot-option-value (illegal-table-slot-option-value)
  ()
  (:report (lambda (c s)
	     (format s "Slot ~a option ~a is missing a value"
		     (slot-name c) (slot-option-name c)))))
