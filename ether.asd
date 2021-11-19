(asdf:defsystem :ether
  :description "A bunch of CL utility functions and macros."
  :author "William Flageol"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "../package")
	       (:file "base")
               (:file "data")
	       (:file "random")
	       (:file "io")
	       (:file "classes")
	       (:file "statistics")
               (:file "time")
               (:file "graph")
               (:file "ether2"))
  :depends-on (:alexandria
	       :cl-fad
	       :cl-ppcre
	       :trivial-garbage
	       :cl-annot
	       :cl-actors
	       :cl-json
	       :lisp-unit
               :iterate))
