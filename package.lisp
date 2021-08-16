(defpackage :ether
  (:use :cl :alexandria :iterate)
  (:import-from :annot.class :export-class)
  (:import-from :lisp-unit :define-test :run-tests)
  (:export :export-class))

(defpackage :ether-io
  (:use :cl :cl-ppcre))

(defpackage :ether2
  (:use :cl :alexandria)
  (:export :maptree :script :script-when :-> :lambda->
   :define-class :trace-break :take :between))
