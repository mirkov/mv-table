;;;; package.lisp

(defpackage #:table-dev
  (:use  #:closer-common-lisp #:lisp-unit)
  (:import-from :alexandria :mappend)
  (:export :push-on-end)
  (:documentation "Table development package.  Meant to be inherited by other table packages, not user code."))

