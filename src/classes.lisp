(in-package :ether)
(annot:enable-annot-syntax)

(defmacro defclass% (class-name superclasses slots &key mutable? legacy?)
  `(defclass ,class-name ,superclasses
     ,(loop for slot in slots
	 collect (let ((slot-name (if (listp slot)
				      (car slot)
				      slot))
		       (init-form (if (listp slot)
				      (cadr slot)
				      nil)))
		   `(,slot-name
		     :initform ,init-form
		     :initarg ,(make-keyword (string slot-name))
		     ,(if mutable? :accessor :reader)
		     ,(if legacy?
			  slot-name
			  (intern
			   (concatenate
			    'string
			    (string class-name)
			    "-"
			    (string slot-name)))))))))

@export
(defun clone (object)
  #+ccl (ccl:copy-instance object)
  #-ccl (error "Clone not implemented yet."))

@export
(defmacro defclassi (class-name superclasses &rest slots)
  "Alternate syntax for defining mutable classes.
(Legacy, use define-class now.)"
  `(progn
     (defclass% ,class-name ,superclasses ,slots :legacy? t)))

@export
(defmacro defclassm (class-name superclasses &rest slots)
  "Alternate syntax for defining mutable classes.
(Legacy, use define-class now."
  `(defclass% ,class-name ,superclasses ,slots :mutable? t :legacy? t))

@export
(defmacro define-constructor (function-name class-name parameters)
  `(defun ,function-name ,parameters
     (make-instance ',class-name
                    ,.(flatten
                       (mapcar (lambda (p)
                                 (let ((param-name (if (listp p) (car p) p)))
                                   (list (make-keyword param-name) param-name)))
                               (remove-if (lambda (p)
                                            (or (eq '&optional p)
                                                (eq '&key p)
                                                (eq '&rest p)))
                                          parameters))))))

@export
(defmacro define-class (class-name (&key
                                      superclasses
                                      mutable?
                                      (constructor 0 constructor?))
			&rest slots)
  "Alternate syntax for defining classes."
  `(progn
     ,(when constructor?
        `(define-constructor ,class-name ,class-name ,constructor))
     (defclass% ,class-name ,superclasses ,slots
       :mutable? ,mutable?)))

(defun %-symbol? (expr)
  (and (symbolp expr)
       (string= (subseq (string expr) 0 1) "%")))

(defun split-%-symbols (expr-list)
  (let (%-symbols other-symbols)
    (loop for expr in expr-list
       do (if (%-symbol? expr)
	      (push expr %-symbols) 
	      (push expr other-symbols)))
    (list %-symbols other-symbols)))

@export
(defmacro with-new (slots instance &body body)
  "Works the same as with-slots, except it creates and returns a new instance instead of mutating the original. 
The instance's class must implement the clone generic method."
  (let ((instance-sym (gensym)))
    `(let ((,instance-sym (clone ,instance)))
       (with-slots ,slots ,instance-sym
         ,@body
         ,instance-sym))))

@export
(defmacro make-new (instance &rest copy-slots)
  `(with-new ,(mapcar #'car copy-slots) ,instance
     ,.(loop for copy-slot in copy-slots
	  collect `(setf ,(car copy-slot) ,(cadr copy-slot)))))

@export
(defmacro map-with-new (slots instances &body body)
  `(mapcar (lambda (i) (with-new ,slots i ,@body)) ,instances))

(defmacro update% (instance new? args)
  (-> (plist-alist args)
      `(,(if new? 'with-new 'with-slots)
        ,(mapcar (lambda (arg) (car arg)) %) ,instance
         ,.(mapcar (lambda (arg)
                     `(setf ,(car arg) ,(cdr arg)))
                   %))))

(defmacro build-nested-access (base-instance properties)
  (if properties
      `(build-nested-access (slot-value ,base-instance ',(car properties))
                            ,(cdr properties))
      base-instance))

@export
(defmacro .-> (base-instance &rest properties)
  `(build-nested-access ,base-instance ,properties))

@export
(defmacro define-equal-function (function-name
                                 test
                                 &rest accessors)
  `(defun ,function-name (o1 o2)
     (and ,.(loop for accessor in accessors
                  collect `(funcall ,test
                                    (slot-value o1 ',accessor)
                                    (slot-value o2 ',accessor))))))

(defun make-let-slot-values (string)
  (-> (search "." string :from-end t)
      (if %
          `(slot-value ,(make-let-slot-values (subseq string 0 %))
                       ',(intern (subseq string (1+ %))))
          (intern string))))

(defmacro single-let-slots (instance-sym-and-form &body body)
  (let* ((instance-sym (car (ensure-list instance-sym-and-form)))
         (instance-form (if (listp instance-sym-and-form)
                            (cadr instance-sym-and-form)
                            instance-sym-and-form))
         (prefix (format nil "~a." (string instance-sym)))
         (prefix-length (length prefix)))
    `(let ((,instance-sym ,instance-form))
       ,.(maptree (lambda (symbol)
                    (let ((string (if (symbolp symbol) (string symbol) "")))
                      (if (and (> (length string) prefix-length)
                               (string= prefix (subseq string 0 prefix-length)))
                          (make-let-slot-values string)
                          symbol)))
                  body))))

@export
(defmacro let-slots ((&rest bindings) &body body)
  (if bindings
      `(single-let-slots ,(car bindings)
         (let-slots ,(cdr bindings) ,@body))
      `(progn ,@body)))

@export
(defmacro let-new-slots (instance-sym-and-form &body body)
  (let* ((instance-sym (car (ensure-list instance-sym-and-form)))
         (instance-form (if (listp instance-sym-and-form)
                            (cadr instance-sym-and-form)
                            instance-sym-and-form)))
    `(single-let-slots (,instance-sym (clone ,instance-form))
       ,@body
       ,instance-sym)))
