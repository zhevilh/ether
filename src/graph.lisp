(defpackage :ether.graph
  (:use :cl :alexandria :ether))
(in-package :ether.graph)
(cl-annot:enable-annot-syntax)

@export-class
(defclass digraph ()
  ((vertices :initform nil :reader digraph-vertices)
   (edges :initform nil :reader digraph-edges)))

(defmethod print-object ((d digraph) stream)
  (let-slots (d)
    (format stream "#<DIGRAPH (~a/~a)>" (length d.vertices) (length d.edges))))

@export-class
(defclass digraph-edge ()
  ((vertex-from :initarg :from :reader edge-from)
   (vertex-to :initarg :to :reader edge-to)
   (weight :initarg :weight :reader edge-weight)))

(defmethod print-object ((e digraph-edge) stream)
  (let-slots (e)
    (format stream "#<DIGRAPH-EDGE (~a ~a ~a)>" e.vertex-from e.vertex-to e.weight)))

@export
(defun digraph ()
  (make-instance 'digraph))

@export
(defun add-vertex (digraph value)
  (let-slots ((d digraph))
    (when (not (find value d.vertices))
      (push value d.vertices))))

(defun edge-equal (edge1 edge2)
  (let-slots ((e1 edge1)
              (e2 edge2))
    (and (eq e1.vertex-from e2.vertex-from)
         (eq e1.vertex-to e2.vertex-to)
         (equal e1.weight e2.weight))))

@export
(defun add-edge (digraph vertex-from vertex-to &optional weight)
  (let-slots ((d digraph))
    (add-vertex digraph vertex-from)
    (add-vertex digraph vertex-to)
    (let ((edge (make-instance 'digraph-edge
                               :from vertex-from
                               :to vertex-to
                               :weight weight)))
      (when (not (find edge d.edges :test #'edge-equal))
        (push edge d.edges)))))

@export
(defun vertex-edges (digraph vertex)
  (let-slots ((d digraph))
    (loop for e in d.edges
          when (eq (.-> e vertex-from) vertex)
            collect e)))

@export
(defun merge-digraphs (&rest digraphs)
  (let ((new (digraph)))
    (dolist (d digraphs)
      (let-slots (d)
        (dolist (v d.vertices)
          (add-vertex new v))
        (dolist (e d.edges)
          (let-slots (e)
            (add-edge new e.vertex-from e.vertex-to e.weight)))))))

@export
(defun digraph->dot (stream digraph &key (name "G") (concentrate? t))
  (format stream "digraph ~a {~%" name)
  (when concentrate?
    (format stream "  concentrate = true;~%"))
  (let-slots ((d digraph))
    (dolist (v d.vertices)
      (format stream "  ~a;~%" v))
    (dolist (e d.edges)
      (let-slots (e)
        (format stream "  ~a -> ~a" e.vertex-from e.vertex-to)
        (when e.weight
          (format stream " [label=~a]" e.weight)))
      (format stream ";~%")))
  (format stream "}"))
