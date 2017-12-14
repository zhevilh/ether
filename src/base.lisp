(in-package :ether)
(annot:enable-annot-syntax)

(defmacro with-shadow-fn (name fn &body form)
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

@export
(defmacro labels! ((&rest bindings) &body body)
  "A dynamic-scope version of labels."
  (if bindings
      (destructuring-bind (name params fn-body) (car bindings)
	`(with-shadow-fn ,name (lambda ,params ,fn-body)
	   (labels! ,(cdr bindings) ,@body)))
      `(progn ,@body)))

(defmacro with-unwind-protect (sym init-form unwind-form &body body)
  `(let ((,sym ,init-form))
     (unwind-protect (progn ,@body)
       ,unwind-form)))

@export
(defmacro let-protect ((&rest bindings) &body body)
  "A let-form with built-in unwind-protect.
Usage: (let-protect ((symbol1 init-form1 unwind-form1)
                     (symbol2 init-form2 unwind-form2)...)
         ...)"
  (if bindings
      (destructuring-bind (symbol init-form unwind-form) (car bindings)
	`(with-unwind-protect ,symbol ,init-form ,unwind-form
	   (let-protect ,(cdr bindings) ,@body)))
      `(progn ,@body)))

@export
(defmacro finalize (object &body finalize-body)
  "A wrapper macro around tg:finalize."
  `(tg:finalize ,object (lambda () ,@finalize-body)))

@export
(defun maptree (fn tree)
  "Same as mapcar, but works on a single tree structure."
  (when tree
    (if (atom tree)
	(funcall fn tree)
	(mapcar (curry 'maptree fn)
		tree))))

@export
(defmacro -> (start-value &rest threaded-functions)
  (if threaded-functions
      `(let ((% ,start-value))
	 (-> ,(maptree (lambda (leaf) (if (and (symbolp leaf)
					       (string= (string leaf) "%"))
					  'ether::%
					  leaf))
		       (car threaded-functions))
	     ,@(cdr threaded-functions)))
      start-value))

(defun count-lambda-dynamic-params (expr)
  (let (params)
    (dolist (leaf (flatten expr))
      (when (and (symbolp leaf) (eq 0 (search "%" (string leaf)))) 
	(push leaf params)))
    (length (remove-duplicates params))))

@export
(defmacro build-lambda (&body expr)
  (let ((expr (maptree (lambda (leaf)
			 (if (and (symbolp leaf)
				  (eq 0 (search "%" (string leaf))))
			     (if (string= (string leaf) "%")
				 '%1 (intern (string leaf) :ether))
			     leaf))
		       expr)))
    `(lambda (,.(loop for i from 1 to (count-lambda-dynamic-params expr)
		   collect (intern (format nil "%~a" i) :ether)))
       ,@expr)))

(defun package-functions (package)
  (let (r
	(package (find-package package)))
    (do-all-symbols (s)
      (when (and (eql (symbol-package s) package)
		 (fboundp s)
		 (not (macro-function s))
		 (not (special-operator-p s)))
	(push s r)))
    r))

@export
(defun take (seq count)
  (subseq seq 0 (min count (length seq))))
