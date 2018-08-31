(in-package #:ether)
(cl-annot:enable-annot-syntax)

@export
(defun randomize ()
  (setf *random-state* (make-random-state t)))

@export
(defun flip ()
  (= (random 2) 0))

(define-test flip
  (flip))

@export
(defun! shuffle (seq)
  (-> (mapcar (lambda (e) (cons e (random 1.0))) seq)
      (sort (copy-seq %) #'> :key #'cdr)
      (mapcar #'car %)))

(define-test shuffle
  (shuffle '(1 2 3)))

@export
(defun pick (seq)
  (car (shuffle seq)))

@export
(defun pick-many (n seq)
  (take (shuffle seq) n))

@export
(defun random-gaussian (mean standard-deviation &key min max)
  (let ((r 0.0))
    (loop
       for retry = 0 then (1+ retry)
       while (or (= retry 0)
		 (and min (< r min))
		 (and max (> r max)))
       do (progn
	    (when (> retry 10000)
	      (error "Could not generate gaussian random value."))
	    (let ((u1 (random 1.0))
		  (u2 (random 1.0)))
	      (setf r (+ (* (sqrt (* -2 (log u1)))
			    (sin (* 2 pi u2))
			    standard-deviation)
			 mean)))))
    r))

(define-test random-gaussian
  (random-gaussian 10 5)
  (random-gaussian 10 5 :min 2)
  (random-gaussian 10 5 :max 15)
  (random-gaussian 10 5 :min 2 :max 15))

@export
(defun decide% (possibilities)
  (when possibilities
    (let* ((possibilities (mapcar (lambda (x) (cons (car x) (max 0 (float (cadr x)))))
                                  possibilities))
	   (total-weight (-> (mapcar #'cdr possibilities)
			     (reduce #'+ %)))
	   (r (random total-weight)))
      (loop for p in possibilities
            do (decf r (cdr p))
            when (< r 0)
              return (car p)))))

@export
(defmacro decide (&rest possibilities)
  `(funcall (decide% (list ,.(mapcar (lambda (x)
                                       (destructuring-bind (key value) x
                                         `(list (lambda () ,key) ,value)))
                                     possibilities)))))

@export
(defun decide-t (t-weight nil-weight)
  (decide (t t-weight) (nil nil-weight)))

(define-test decide
  (decide (:a 1) (:b 2) (:c -3))
  (decide))
