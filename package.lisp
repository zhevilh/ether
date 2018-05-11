(defpackage :ether
  (:use :cl :alexandria :cl-actors)
  (:import-from :fset
		:@ :lookup :$)
  (:import-from :annot.class
		:export-class)
  (:import-from :lisp-unit :define-test :run-tests)
  (:export :export-class))

(defpackage :ether-io
  (:use :cl :cl-ppcre))

(defpackage :ether-immutables
  (:use :cl :ether :fset)
  (:shadowing-import-from :fset
			  #:set #:map
			  #:union #:intersection #:set-difference #:complement
			  #:first #:last #:subseq #:reverse #:sort #:stable-sort
			  #:reduce
			  #:find #:find-if #:find-if-not
			  #:count #:count-if #:count-if-not
			  #:position #:position-if #:position-if-not
			  #:remove #:remove-if #:remove-if-not
			  #:substitute #:substitute-if #:substitute-if-not
			  #:some #:every #:notany #:notevery)
  (:export :shadow-import-fset
	   :defmap
	   :empty-map
	   :newmap
	   :map-assoc
	   :with-values
	   :mapcar-value
	   :@))
