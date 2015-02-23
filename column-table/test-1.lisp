(in-package :cl-user)

(defclass mixin ()
  ((slot1)))

(defclass user (mixin)
  ())

(make-instance 'user)
