(in-package :ether-io)

(export '(file->string
	  string->file))

(defun file->string (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun string->file (string filename &key (if-exists :supersede))
  (with-open-file (stream filename
			  :direction :output
			  :if-exists if-exists)
    (with-standard-io-syntax
      (format stream string))))
