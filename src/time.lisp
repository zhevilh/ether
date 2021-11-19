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
(defun make-timer (&key (start-at 0) (start? t))
  (make-instance
   'timer
   :start-time (if start?
                   (- (get-internal-real-time)
                      (* start-at internal-time-units-per-second)))))

@export
(defun start-timer (timer)
  (with-new (start-time) timer
    (setf start-time (get-internal-real-time))))

@export
(defun pause-timer (timer)
  (with-new (freeze-time) timer
    (if (not freeze-time)
        (setf freeze-time (get-internal-real-time)))))

@export
(defun resume-timer (timer)
  (with-new (start-time freeze-time) timer
    (when freeze-time
      (incf start-time (- (get-internal-real-time) freeze-time))
      (setf freeze-time nil))))

@export
(defun push-timer (timer time)
  (with-new (start-time) timer
    (decf start-time (* time internal-time-units-per-second))))

@export
(defun timer-time (timer)
  (with-slots (start-time freeze-time) timer
    (float (/ (- (or freeze-time (get-internal-real-time))
                 start-time)
              internal-time-units-per-second))))

@export
(defun timer-started? (timer)
  (and (not (eq nil (.-> timer start-time)))
       (not (.-> timer freeze-time))))
