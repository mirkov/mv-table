(in-package :mv-table)

(defun canonicalize-deftable-options (options)
  ;; after canonicalize-defclass-options tAotMOP, p. 287
  (mappend #'canonicalize-deftable-option options))

(defun canonicalize-deftable-option (option)
  "Check that option is a valid table option and bring it to standard
form.  Option is a list"
  ;; after canonicalize-defclass-option tAotMOP, p. 287, but adapted
  ;; to tables to check for certain invalid options.  The other
  ;; options are passed unmodified
  (case (car option)
    (:metaclass
     (error ":METACLASS option is not allowed"))
    (:default-initargs
     (error ":default-initargs option is not allowed"))
    ;; not clear as
    (t (list `',(car option) `',(cadr option)))))
