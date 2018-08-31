(in-package :ether)
(cl-annot:enable-annot-syntax)

(define-class timer ()
  start-time
  freeze-time)

@export
(defun timer (&optional (start? t))
  (make-instance
   'timer
   :start-time (if start? (get-internal-real-time))))

@export
(defun! start-timer (timer)
  (with-new (start-time) timer
    (setf start-time (get-internal-real-time))))

@export
(defun! pause-timer (timer)
  (with-new (freeze-time) timer
    (setf freeze-time (get-internal-real-time))))

@export
(defun! resume-timer (timer)
  (with-new (start-time freeze-time) timer
    (incf start-time (- (get-internal-real-time) freeze-time))
    (setf freeze-time nil)))

@export
(defun timer-time (timer)
  (with-slots (start-time freeze-time) timer
    (float (/ (- (or freeze-time (get-internal-real-time))
                 start-time)
              internal-time-units-per-second))))
