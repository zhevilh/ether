(in-package :ether)

(defmacro with-shadow-fn ((name fn) &body form)
  "Shadows the function named \"name\" with \"fn\".
Based on answer to http://stackoverflow.com/questions/3074812/common-lisp-redefine-an-existing-function-within-a-scope."
  (with-gensyms (base-fn)
    `(let (,base-fn)
       (cond ((fboundp ',name)
	      (setf ,base-fn (symbol-function ',name))
	      (setf (symbol-function ',name) ,fn)
	      (unwind-protect (progn ,@form)
		(setf (symbol-function ',name) ,base-fn)))
	     (t
	      (setf (symbol-function ',name) ,fn)
	      (unwind-protect (progn ,@form)
		(fmakunbound ',name)))))))

(export 'labels!)
(defmacro labels! ((&rest bindings) &body body)
  "A dynamic-scope version of labels."
  (if bindings
      (let ((fn-def (car bindings)))
	`(with-shadow-fn (,(car fn-def) (lambda ,(cadr fn-def) ,(caddr fn-def)))
	   (labels! ,(cdr bindings) ,@body)))
      `(progn ,@body)))
