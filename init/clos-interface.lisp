(in-package :table-dev)


;;; Table classes and generic functions
(defclass table ()
  ((device :initform (error 'unspecified-device))
   (direction :initform (error 'unspecified-direction))
   (format :initform (error 'unspecified-format)))
  (:documentation "Base class for all tables"))

(defclass table-row ()
  ()
  (:documentation "Base class for table row accessors"))

(defclass table-col ()
  ()
  (:documentation "Base class for table col accessors"))

(defgeneric make-table (type &key)
  (:documentation "General interface for making a table instance")
  (:method ((table-type t) &key)
    (error 'undefined-table-type :table-type table-type)))

(defgeneric table-row (table row-identifier &key)
  (:documentation "Generalized accessor to a table row.  Returns an
  object that is a sub-type of TABLE-ROW"))

(defgeneric table-col (table col-identifier &key)
  (:documentation "Generalized accessor to a table col.  Returns an
  object that is a sub-type of TABLE-ROW"))

(defgeneric table-ref (table row-identifier col-identifier &key)
  (:documentation "Generalized accessor to a table cell.  Accesses the
  cell value"))

(defgeneric num-rows (table)
  (:documentation "Return the number of table rows"))

(defgeneric num-cols (table)
  (:documentation "Return the number of table columns"))

(defgeneric col-identifier (table-storage col-schema col-name)
  (:documentation "Return an identifier for the column that can be
  used by 2DREF, when passed the table, or the table storage

For arrays, the col-identifier will return an index.  For hash based
storage, it will return a key

TABLE-STORAGE is either a table or the storage that the table is using
COL-SCHEMA is the table column schema
COL-NAME is how we access this column.  It can be a symbol, an index

Internally, COL-NAME will be compared to entries in COL-SCHEMA using
its EQUALITY-PREDICATE "))

(defgeneric row-identifier (table-storage row-schema row-name)
  (:documentation "Return an identifier for the row that can be
  used by 2DREF, when passed the table, or the table storage

For arrays, the row-identifier will return an index.  For hash based
storage, it will return a key.

See documentation for COL-IDENTIFIER for more info"))

;;; Classes that define table properties

;; Format
(defclass table-format () ())

;;; We do not define any table formats here.  They are defined in
;;; their separate modules.  But each format must be a sub-class of
;;; table-format.  As an example:
#|
 (defclass square-table (table-format)
  ( ... square table slots ...))
|#


;; Direction
(defclass table-direction () ())

(defclass input-table (table-direction)
  ((direction :initform :input)))

(defclass output-table (table-direction)
  ((direction :initform :input)))


(defclass bidirectional-table (input-table output-table)
  ((direction :initform :input)))


;; Define devices

(defclass table-device () ())
(defclass native-device (table-device)
  ((device :initform :CL)))

(defclass file-device (table-device)
  ((device :initform :disk)
   (pathname :initarg :pathname
	     :reader disk-pathname)))

(defclass remote-device ()
  ((device :initform :network)
   (port :initarg :port
	 :reader port)))

;; Define protocol

(defclass table-protocol () ())
(defclass native-protocol (table-protocol) ())
(defclass csv-protocol (table-protocol) ())

;; Classes used for testing
(defclass test-format (table-format) ())
(defclass test-device (table-device) ())
(defclass test-direction (table-direction) ())
(defclass test-protocol (table-protocol) ())

;;; 1D schemas
(defclass 1d-schema ()
  ((count :initform 0)
   (names :initform (make-array 0 :adjustable t :fill-pointer 0)
	  :accessor names
	  :documentation "Data structure that stores the names")
   (equality-predicate :initarg :equality-predicate
		       :accessor equality-predicate
		       :documentation "Function of two arguments for testing
the equality against name.  By default it is EQUALP")
   #+skip(comparator :initarg :comparator
	       :accessor comparator)
   (value-normalizer :initarg :value-normalizer
		     :accessor value-normalizer
		     :documentation
"Function of two arguments, the value, and the schema"))
  (:documentation "Base schema for 1D objects, such as:
- row or column headers of square tables
- columns of column tables

Extensions to this schema may provide facilities for comparing rows or
columns")
  (:default-initargs
   :value-normalizer #'pass-through
    :equality-predicate #'equalp))



;;; Other

(defgeneric display-table (stream format table)
  (:documentation "Display TABLE according to FORMAT to STREAM"))
