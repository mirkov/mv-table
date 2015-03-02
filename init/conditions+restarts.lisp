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
