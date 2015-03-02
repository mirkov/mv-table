(in-package :table-dev)

;; These must have a value of one of the classes that devine 
(defparameter *default-format* 'square-table)
(defparameter *default-direction* 'bidirectional-table)
(defparameter *default-protocol* 'native-protocol)
(defparameter *default-device* 'native-device)
(defparameter *square-table-default-storage* 'array
  "Can be an ARRAY, FOREIGN-ARRAY, NESTED-HASH")
