(asdf:defsystem :ether
  :serial t
  :pathname "src"
  :components ((:file "../package")
	       (:file "base")
	       (:file "io")
	       (:file "classes"))
  :depends-on (:fset :alexandria))
