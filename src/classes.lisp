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
(defmacro define-class (class-name (&key superclasses
					 mutable?)
			&rest slots)
  "Alternate syntax for defining classes."
  `(progn
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
      `(with-slots (,(car properties)) ,base-instance
         (build-nested-access ,(car properties) ,(cdr properties)))
      base-instance))

@export
(defmacro .-> (base-instance &rest properties)
  `(build-nested-access ,base-instance ,properties))
