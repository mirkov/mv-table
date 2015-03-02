(in-package :table-dev)

(defun pass-through (value &optional extra)
  "Return VALUE, discard EXTRA

Used as the default value-normalizer"
  (declare (ignorable extra))
  value)
