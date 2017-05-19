(in-package :ether)
(annot:enable-annot-syntax)

(defmacro defclass% (class-name superclasses slots &key (mutable? nil))
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
		     ,(if mutable? :accessor :reader) ,slot-name)))))

@export
(defun clone (object)
  #+ccl (ccl:copy-instance object)
  #-ccl (error "Clone not implemented yet."))

@export
(defmacro defclassi (class-name superclasses &rest slots)
  "Alternate syntax for defining mutable classes."
  `(progn
     (defclass% ,class-name ,superclasses ,slots)))

@export
(defmacro defclassm (class-name superclasses &rest slots)
  "Alternate syntax for defining mutable classes."
  `(defclass% ,class-name ,superclasses ,slots :mutable? t))

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
