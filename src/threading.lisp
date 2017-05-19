(in-package :ether)
(annot:enable-annot-syntax)

@export
(defactor queue-thread () (fn)
  (funcall fn)
  next)

@export
(defmacro queue-action (queue-thread &body action)
  `(send ,queue-thread (lambda () ,@action)))

@export
(defun kill-queue-thread (queue-thread)
  (stop-actor queue-thread))
