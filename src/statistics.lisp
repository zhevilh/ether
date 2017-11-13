(in-package :ether)

(annot:enable-annot-syntax)

@export
(defun plot-function (f &optional intervals n)
  (let* ((intervals (or intervals 10))
	 (n (or n 10000))
	 (values (loop for x from 1 to n collect (funcall f)))
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
       do (progn  (format t "~10f: " (+ min (* (+ 0.5 j) interval)))
		  (dotimes (i (round (* 75 (/ c n))))
		    (format t "|"))
		  (format t "~%")))
    (format t "Min: ~f~%" min)
    (format t "Max: ~f~%" max)
    (format t "Mean: ~f~%" (mean values))
    (format t "Variance: ~f~%" (variance values))
    (format t "Standard deviation: ~f~%" (standard-deviation values))))

@export
(defmacro plot-expr ((&key intervals n) &body expr)
  `(plot-function (lambda () ,@expr) ,intervals ,n))

@export
(defun plot-discrete-function (f &optional sort-f n)
  (let* ((n (or n 10000))
	 (values (loop for x from 1 to n collect (funcall f)))
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
	  (format t "~15a: " (if (> (length value) 15)
				 (subseq value 0 15)
				 value)))
	(dotimes (i (round (* 65 (/ (cdr a) n))))
	  (format t "|"))
	(format t "~%")))))

@export
(defmacro plot-discrete-expr ((&key sort-f n) &body expr)
  `(plot-discrete-function (lambda () ,@expr) ,sort-f ,n))

@export
(defun gaussian-function (a b c x)
  (* a (exp (- (/ (expt (- x b) 2)
		  (* 2 (expt c 2)))))))

@export
(defun gaussian-random-custom (mean standard-deviation &optional min max)
  (+ mean
     (* standard-deviation
	(gaussian-random (when min (/ (- min mean) standard-deviation))
			 (when max (/ (- max mean) standard-deviation))))))
