(in-package :ether-io)
(annot:enable-annot-syntax)

@export
(defun file->string (filename &key (encoding :UTF-8))
  (with-open-file (stream filename
                          :external-format encoding)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

@export
(defun string->file (string filename &key
				       (if-exists :supersede)
				       (encoding :UTF-8))
  (with-open-file (stream filename
			  :direction :output
			  :if-exists if-exists
			  :external-format encoding)
    (with-standard-io-syntax
      (format stream "~a" string))))

@export
(defun list-directory (dirname &key file-type recursive?)
  (let ((dirname (truename dirname))
        (file-types (if (listp file-type) file-type (list file-type))))
    (loop for entry in (cl-fad:list-directory dirname)
          append (cond ((cl-fad:directory-pathname-p entry)
                        (when recursive?
                          (list-directory entry
                                          :file-type file-type
                                          :recursive? t)))
                       ((or (not file-types)
                            (find (pathname-type entry) file-types
                                  :test #'string-equal))
                        (list entry))))))

@export
(defun common-directory (directory1 directory2)
  (loop for part1 in (cdr (pathname-directory directory1))
        for part2 in (cdr (pathname-directory directory2))
        while (string= part1 part2)
        collect part1 into parts
        finally (return
                  (format nil "~a:/~{~a/~}" (pathname-device directory1) parts))))

@export
(defun namestring= (string1 string2)
  (string= (namestring string1) (namestring string2)))
