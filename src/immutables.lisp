(in-package :ether-immutables)

(defmacro defmap (name &body properties)
  `(defun ,name (,@properties)
     (map ,.(mapcar (build-lambda
		      (let ((sym (if (listp %) (car %) %)))
			(list (alexandria:make-keyword
			       (string sym))
			      sym)))
		    (remove-if (build-lambda
				 (or (eq '&optional %)
				     (eq '&key %)))
			       properties)))))
