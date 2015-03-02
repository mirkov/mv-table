;;;; package.lisp

(defpackage #:table-dev
  (:use  #:closer-common-lisp #:alexandria #:grid #:lisp-unit)
  (:shadowing-import-from :alexandria :set-equal)
  (:shadowing-import-from :grid :norm :aref)
  (:export :push-on-end)
  (:documentation "Table development package.  Meant to be inherited by other table packages, not user code."))

