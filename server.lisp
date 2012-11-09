(in-package #:wimelib-archivist)

(defvar *server* (make-instance 'easy-acceptor :port 4243))

(defun start-server ()
  (setf *database* (open-db *database-path*))
  (start *server*))

(defun stop-server ()
  (stop *server*)
  (close-db *database*)
  (setf *database* nil))
