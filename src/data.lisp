(in-package :ether)

;; Hashmap reader syntax
(set-macro-character
 #\{
 (lambda (stream char)
   (declare (ignore char))
   (let ((*readtable* (copy-readtable *readtable* nil))
	 (keep-going t))
     (set-macro-character #\} (lambda (stream char)
				(declare (ignore char) (ignore stream))
				(setf keep-going nil)))
     `(fset:map 
       ,.(loop
	    for key = (read stream nil nil t)
	    while keep-going
	    for value = (read stream nil nil t)
	    collect (list key value))))))

;; Sequence reader syntax
(set-macro-character
 #\[
 (lambda (stream char)
   (declare (ignore char))
   (let ((*readtable* (copy-readtable *readtable* nil))
	 (keep-going t))
     (set-macro-character #\] (lambda (stream char)
				(declare (ignore char) (ignore stream))
				(setf keep-going nil)))
     `(fset:seq
       ,.(loop
	    for value = (read stream nil nil t)
	    while keep-going
	    collect value)))))

(defun ++ (base-collection append-collection)
  (apply #'fset:with base-collection (fset:convert 'list append-collection)))
