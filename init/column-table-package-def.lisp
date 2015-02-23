;;;; package.lisp

(defpackage #:column-table
  (:use  #:closer-common-lisp #:lisp-unit #:table-dev)
  (:import-from :alexandria :mappend)
  (:export :def-column-table
	   :make-column-table
	   :write-table
	   :read-table)
  (:documentation "Top level column-table package with user interface"))

