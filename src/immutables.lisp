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
				     (eq '&key %)
				     (eq '&rest %)))
			       properties)))))

(defconstant empty-map (map))

(defmacro newmap (key value &rest kvs)
  `(map (,key ,value)
	,.(mapcar (build-lambda `(,(car %) ,(cdr %)))
		  (alexandria:plist-alist kvs))))

(defun map-assoc (map key val &rest kvs)
  (reduce (build-lambda (with %1 (car %2) (cdr %2)))
	  (alexandria:plist-alist kvs) :initial-value (with map key val)))

(defun build-nested-lookup (base-map keys)
  (if keys
      `(lookup ,(build-nested-lookup base-map (cdr keys)) ,(car keys))
      base-map))

(defmacro with-values ((&rest value-entries) map &body body)
  (let (sks)
    (dolist (ve value-entries)
      (push (if (listp ve) ve (list ve (alexandria:make-keyword (string ve))))
	    sks))
    `(symbol-macrolet (,.(mapcar (build-lambda `(,(car %)
						  ,(build-nested-lookup map (reverse (cdr %)))))
			   sks))
       ,@body)))

(defmacro mapcar-value (maps &rest keys)
  `(mapcar (build-lambda ,(build-nested-lookup '% (reverse keys)))
	   ,maps))

(defmacro @ (map &rest keys)
    "Redefined @ macro from fset because it is incomplete and error-prone.
This one doesn't try to act like funcall, but can access nested maps."
  (build-nested-lookup map (reverse keys)))
