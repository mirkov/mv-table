(in-package :table-dev)

(defmacro push-on-end (value location)
  "Append value to location.  Like push, except on the tail-end

From Closette code
http://homepage.stat.uiowa.edu/~luke/xls/xlispstat/other/closette/closette.lisp"
  `(setf ,location (nconc ,location (list ,value))))
