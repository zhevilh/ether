(in-package :ether)

(annot:enable-annot-syntax)

@export
(defun plot-function (f &optional intervals n)
  (plot (loop for x from 1 to (or n 10000) collect (funcall f))
        :intervals intervals))

@export
(defun plot (values &key intervals (stream t))
  (let* ((intervals (or intervals 10))
	 (n (length values))
	 (min (apply #'min values))
	 (max (apply #'max values))
	 (interval (/ (- max min) intervals))
	 (graph (make-array intervals)))
    (if (= interval 0)
	(setf (elt graph 0) n)
	(dolist (v values)
	  (incf (elt graph (min 9 (floor (- v min) interval))))))
    (loop for c across graph
          for j = 0 then (1+ j)
          do (progn  (format stream "~10f: " (+ min (* (+ 0.5 j) interval)))
                     (dotimes (i (round (* 75 (/ c n))))
                       (format stream "|"))
                     (format stream "~%")))
    (format stream "Count: ~a~%" n)
    (format stream "Min: ~f~%" min)
    (format stream "Max: ~f~%" max)
    (format stream "Mean: ~f~%" (mean values))
    (format stream "Median: ~f~%" (median values))
    (format stream "Variance: ~f~%" (variance values))
    (format stream "Standard deviation: ~f~%" (standard-deviation values))))

@export
(defmacro plot-expr ((&key intervals n) &body expr)
  `(plot-function (lambda () ,@expr) ,intervals ,n))

@export
(defun plot-discrete-function (f &optional sort-f n print-f)
  (plot-discrete (loop for x from 1 to (or n 10000) collect (funcall f))
                 :sort-f sort-f :print-f print-f))

@export
(defun plot-discrete (values &key sort-f print-f (stream t)
                               count?)
  (let ((n (length values))
        graph)
    (dolist (v values)
      (if (getf graph v)
	  (incf (getf graph v))
	  (setf (getf graph v) 1)))
    (setf graph (plist-alist graph))
    (let ((sample (car values)))
      (setf graph (sort graph
			(lambda (l g)
			  (funcall (or sort-f
				       (and (numberp sample) #'<)
				       (build-lambda (string< (string %1) (string %2))))
				   (car l) (car g))))))
    (dolist (a graph)
      (progn
	(let ((value (write-to-string (car a))))
	  (format stream "~15a: " (-> (if print-f (funcall print-f value) value)
                                 (if (> (length %) 15)
                                     (subseq % 0 15)
                                     %))))
	(dotimes (i (round (* 65 (/ (cdr a) n))))
	  (format stream "|"))
        (if count?
            (format stream " (~a)" (cdr a))
            (format stream " (~,2f%)" (* 100 (/ (cdr a) n))))
	(format stream "~%")))))

@export
(defmacro plot-discrete-expr ((&key sort-f n print-f) &body expr)
  `(plot-discrete-function (lambda () ,@expr) ,sort-f ,n ,print-f))
