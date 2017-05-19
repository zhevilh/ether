(in-package :ether-io)
(annot:enable-annot-syntax)

@export
(defun file->string! (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

@export
(defun string->file! (string filename &key (if-exists :supersede))
  (with-open-file (stream filename
			  :direction :output
			  :if-exists if-exists)
    (with-standard-io-syntax
      (format stream string))))

@export
(defun list-directory! (dirname &key file-type)
  (let* ((dirname (truename dirname))
	 (files (cl-fad:list-directory dirname)))
    (if file-type
	(remove-if-not
	 (lambda (file) (scan
			 (format nil "\\.~a$" file-type)
			 (file-namestring file)))
	 files)
	files)))
