(in-package :table-dev)

(defmacro do-table-rows ((row table &optional result) &body body)
  (with-gensyms (row-index num-rows)
  `(let ((,row (table-row ,table ,0))
	 (,num-rows (num-rows ,table)))
     (do ((,row-index 0 (incf ,row-index)))
	 ((= ,row-index ,num-rows) ,@(when result `(,result)))
       (setf (row-index ,row) ,row-index)
       ,@body))))


(defmacro do-table-cols ((col table &optional result) &body body)
  (with-gensyms (col-index num-cols)
  `(let ((,col (table-col ,table ,0))
	 (,num-cols (num-cols ,table)))
     (do ((,col-index 0 (incf ,col-index)))
	 ((= ,col-index ,num-cols) ,@(when result `(,result)))
       (setf (col-index ,col) ,col-index)
       ,@body))))
