(defpackage :ether
  (:use :cl :alexandria :cl-actors)
  (:import-from :fset
		:@ :lookup :$)
  (:import-from :annot.class
		:export-class)
  (:export :export-class))

(defpackage :ether-io
  (:use :cl :cl-ppcre))
