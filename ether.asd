(asdf:defsystem :ether
  :description "A bunch of CL utility functions and macros."
  :author "William Flageol"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "../package")
	       (:file "base")
	       (:file "random")
	       (:file "io")
	       (:file "classes")
	       (:file "threading")
	       (:file "statistics")
               (:file "time")
               (:file "ether2"))
  :depends-on (:alexandria
	       :cl-fad
	       :cl-ppcre
	       :trivial-garbage
	       :cl-annot
	       :cl-actors
	       :cl-json
	       :lisp-unit))
