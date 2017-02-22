(in-package :ether)

(export '(with-new
	  copy-instance))

(defun copy-instance (instance)
  #+ccl (ccl:copy-instance instance)
  #-(or ccl) (error "Not implemented"))

(defmacro with-new (slots instance &body body)
  (let ((instance-sym (gensym)))
    `(let ((,instance-sym (copy-instance ,instance)))
       (with-slots ,slots ,instance-sym
	 ,@body
	 ,instance-sym))))
