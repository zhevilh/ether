(in-package :ether)
(cl-annot:enable-annot-syntax)

@export
(defmacro with-hashes (hash-entries hash &body body)
  (with-gensyms (source)
    `(let ((,source ,hash))
       (declare (ignorable ,source))
       (symbol-macrolet (,.(loop for (sym key) in hash-entries
                                 collect `(,sym (gethash ,key ,hash))))
         ,@body))))
