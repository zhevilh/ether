(in-package :ether)
(annot:enable-annot-syntax)

@export
(defun funcall-base (&rest params)
  (declare (ignore params))
  (error "Cannot be used outside of labels!"))

(defmacro with-shadow-fn (name fn &body form)
  "Shadows the function named \"name\" with \"fn\".
Based on answer to http://stackoverflow.com/questions/3074812/common-lisp-redefine-an-existing-function-within-a-scope."
  (with-gensyms (base-fn)
    `(let (,base-fn)
       (flet ((funcall-base (&rest params)
                (apply #'funcall ,base-fn params)))
         (cond ((fboundp ',name)
                (setf ,base-fn (symbol-function ',name))
                (setf (symbol-function ',name) ,fn)
                (unwind-protect (progn ,@form)
                  (setf (symbol-function ',name) ,base-fn)))
               (t
                (setf (symbol-function ',name) ,fn)
                (unwind-protect (progn ,@form)
                  (fmakunbound ',name))))))))

@export
(defmacro labels! ((&rest bindings) &body body)
  "A dynamic-scope version of labels."
  (if bindings
      (destructuring-bind (name params . fn-body) (car bindings)
	`(with-shadow-fn ,name (lambda ,params ,@fn-body)
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
(defun tree-depth (tree)
  (if (some #'listp tree)
      (1+ (tree-depth (mapcar #'ensure-car tree)))
      1))

(defun make-arrow (caller-macro binding-type start-value threaded-functions)
  (if threaded-functions
      `(,binding-type
	((% ,start-value))
	(,caller-macro
         ,(maptree (lambda (leaf) (if (and (symbolp leaf)
                                           (string= (string leaf) "%"))
                                      'ether::%
                                      leaf))
                   (car threaded-functions))
         ,@(cdr threaded-functions)))
      start-value))

@export
(defmacro -> (start-value &rest threaded-functions)
  (make-arrow '-> 'let start-value threaded-functions))

@export
(defmacro when-> (start-value &rest threaded-functions)
  (make-arrow 'when-> 'when-let start-value threaded-functions))

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
  (when seq
    (subseq seq 0 (min count (length seq)))))

@export
(defun replicate (n &rest values)
  (apply #'concatenate 'list (make-list n :initial-element values)))

@export
(defmacro init (n &body init-form)
  (with-gensyms (i)
    `(init-i ,i ,n ,@init-form)))

@export
(defmacro init-i (i n &body init-form)
  `(loop for ,i from 1 to ,n
         collect (progn ,@init-form)))

@export
(defun cycle (n list)
  (if (< n 0)
      (loop
	 with length = (length list)
	 for i from 1 to (abs n)
	 do (setf list (append (last list) (subseq list 0 (1- length)))))
      (loop for i from 1 to n
	 do (setf list (append (cdr list) (list (car list))))))
  list)

@export
(defun cartesian-product (&rest lists)
  (if (= (length lists) 1)
      (mapcar #'list (car lists))
      (loop for e1 in (car lists)
            append (loop for e2 in (apply #'cartesian-product (cdr lists))
                         collect (cons e1 e2)))))

@export
(defun powerset (list &optional (n (length list)))
  (if list
      (mapcan (lambda (x) (if (< (length x) n)
                              (list (cons (car list) x) x)
                              (list x)))
              (powerset (cdr list) n))
      '(())))

@export
(defun permute (list)
  (if list
      (loop for x in list
            append (mapcar (lambda (y) (cons x y))
                           (permute (remove x list))))
      '(())))

@export
(defun product (list1 list2)
  (loop for e1 in list1
        append (loop for e2 in list2
                     collect (list e1 e2))))

@export
(defun count-consecutive (item sequence
                          &key
                            from-end (test #'eq) (key (lambda (o) o)))
  (loop with counts
        with counting?
        for el in sequence
        do (if (funcall test item (funcall key el))
               (if counting?
                   (incf (car counts))
                   (progn
                     (setf counting? t)
                     (push 1 counts)))
               (setf counting? nil))
        finally (return (if from-end counts (nreverse counts)))))

@export
(defun partition (list &rest lambdas)
  (loop with partitioned = (make-list (1+ (length lambdas)))
        for element in list
        do (push element (elt partitioned
                              (or (position-if (lambda (l) (funcall l element))
                                               lambdas)
                                  (length lambdas))))
        finally (return partitioned)))

@export
(defun abs- (value1 value2)
  (abs (- value1 value2)))

@export
(defmacro clamp! (number min max)
  `(setf ,number (clamp ,number ,min ,max)))

@export
(defun snap (points value &optional floor)
  (loop for p in points
     for target = (cons (abs- value p) p)
     then (let ((delta (abs- value p)))
	    (if (and (< delta (car target))
		     (or (not floor)
			 (>= 0 (- p value))))
		(cons delta p)
		target))
     finally (return (cdr target))))

@export
(defun between (number bound1 bound2)
  (destructuring-bind (min max) (sort (list bound1 bound2) #'<)
    (and (>= number min)
	 (<= number max))))

(defmacro case-between% (key allow-otherwise? on-error forms)
  (once-only (key)
    `(cond
       ,.(append (mapcar (lambda (form)
                           (if (and allow-otherwise? (eq (car form) 'otherwise))
                               `(t ,.(cdr form))
                               `((between ,key ,(caar form) ,(cadar form))
                                 ,.(cdr form))))
                         forms)
                 `((t ,on-error))))))

@export
(defmacro case-between (key &body forms)
  `(case-between% ,key t nil ,forms))

@export
(defmacro ecase-between (key &body forms)
  `(case-between% ,key nil (error "No matching type in ecase-between.") ,forms))

@export
(defmacro trace-break (&rest form)
  (with-gensyms ((return-value "RETURN-VALUE"))
    `(let ((,return-value ,@form))
       (break "Breaking on ~a" ',@form)
       ,return-value)))

@export
(defmacro defun! (spec (binding-arg &rest args) &body body)
  `(progn
     (defun ,spec (,binding-arg ,@args) ,@body)
     (defmacro ,(intern (format nil "~a!" (string spec))) (,binding-arg ,@args)
       `(setf ,,binding-arg (,',spec ,,binding-arg ,,@args)))))

@export
(defun group-by (list key &key (test #'eq))
  (loop with groups
        for el in list
        for g = (find (funcall key el) groups :test test :key (compose key #'car))
        do (if g
               (setf (cdr g) (cons el (cdr g)))
               (push (list el) groups))
        finally (return (reverse groups))))

@export
(defun group-by-index (list key)
  (loop for g
          in (group-by (loop for el in list
                             for i = 0 then (1+ i)
                             collect (cons i el))
                       (compose key #'car))
        collect (mapcar #'cdr g)))

@export
(defun list-not-nil (&rest args)
  (remove nil args))

@export
(defun! list-insert (list index value)
  (append (subseq list 0 index)
          (list value)
          (subseq list index)))

@export
(defun! list-remove (list index)
  (append (subseq list 0 index)
          (subseq list (1+ index))))

@export
(defun! list-replace (list index value)
  (append (subseq list 0 index)
          (list value)
          (subseq list (1+ index))))

@export
(defmacro append! (base-list &rest lists)
  `(setf ,base-list (append ,base-list ,@lists)))

@export
(defmacro push-last (value place)
  `(append! ,place (list ,value))) 


@export
(defun ^ (base &rest exponents)
  (reduce #'expt exponents :initial-value base))
