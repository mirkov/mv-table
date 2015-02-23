;;;; package.lisp

(defpackage #:mv-table
  (:use  #:closer-common-lisp #:lisp-unit)
  (:import-from :alexandria :mappend)
  (:export :deftable
	   :make-table-instance
	   :make-table
	   :write-table
	   :read-table)
  (:documentation "Top level mv-table package with user interface"))

