(in-package :ether2)

(defun maptree (fn tree)
  "Same as mapcar, but works on a single tree structure."
  (when tree
    (if (atom tree)
	(funcall fn tree)
	(mapcar (curry 'maptree fn)
		tree))))

(defun make-script (caller-macro binding-type start-value threaded-functions)
  (if threaded-functions
      `(,binding-type
	((* ,start-value))
	(,caller-macro
         ,(maptree (lambda (leaf) (if (and (symbolp leaf)
                                           (string= (string leaf) "*"))
                                      'ether2::*
                                      leaf))
                   (car threaded-functions))
         ,@(cdr threaded-functions)))
      start-value))

(defmacro script (start-value &rest threaded-functions)
  (make-script 'script 'let start-value threaded-functions))

(defmacro script-when (start-value &rest threaded-functions)
  (make-script 'script-when 'when-let start-value threaded-functions))

(defmacro build-nested-access (base-instance properties)
  (if properties
      `(build-nested-access (slot-value ,base-instance ',(car properties))
                            ,(cdr properties))
      base-instance))

(defmacro -> (base-instance &rest properties)
  `(build-nested-access ,base-instance ,properties))

(defmacro lambda-> (&rest properties)
  `(lambda (base-instance) (-> base-instance ,@properties)))

(defun build-slot (readers writers accessors initargs options)
  (destructuring-bind (name &key
                              (reader nil reader?)
                              (writer nil writer?)
                              (accessor nil accessor?)
                              (allocation nil allocation?)
                              (initform nil initform?)
                              (initarg nil initarg?)
                              (type nil type?)
                              (documentation nil documentation?))
      (ensure-list options)
    (flet ((format-option (option)
             (string-upcase (format nil (if (eq option t) "~a" option) name))))
      (setf reader (if reader? reader (when readers
                                        (setf reader? t)
                                        (intern (format-option readers))))
            writer (if writer? writer (when writers
                                        (setf writer? t)
                                        (intern (format-option writers))))
            accessor (if accessor? accessor (when accessors
                                              (setf accessor? t)
                                              (intern (format-option accessors))))
            initarg (if initarg? initarg (when initargs
                                           (setf initarg? t)
                                           (make-keyword (format-option initargs))))))
    (append `(,name)
            (if reader? `(:reader ,reader))
            (if writer? `(:writer ,writer))
            (if accessor? `(:accessor ,accessor))
            (if allocation? `(:allocation ,allocation))
            (if initform? `(:initform ,initform))
            (if initarg? `(:initarg ,initarg))
            (if type? `(:type ,type))
            (if documentation? `(:documentation ,documentation)))))

(defmacro define-class (class-name superclasses
                        (&key readers writers accessors initargs)
                        slots &rest class-options)
  `(defclass ,class-name ,superclasses
     ,(mapcar (curry #'build-slot readers writers accessors initargs) slots)
     ,@class-options))

(defmacro trace-break (&rest form)
  (with-gensyms ((return-value "RETURN-VALUE"))
    `(let ((,return-value ,@form))
       (break "Breaking on ~a" ',@form)
       ,return-value)))

(defun take (seq count)
  (subseq seq 0 (min count (length seq))))

(setf (fdefinition 'between) #'ether:between)
