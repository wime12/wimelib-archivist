;;;; wimelib-archivist.asd

(asdf:defsystem #:wimelib-archivist
  :serial t
  :description "A manager of scores"
  :author "Wilfried Meindl <wilfried.meindl@gmail.com>"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:parenscript
               #:monkeylib-html
               #:wimelib-utilities
               #:wimelib-sqlite3)
  :components ((:file "package")
	       (:file "database")
               (:file "tables")
	       (:file "handler-macros")
	       (:file "data-acquisition")
	       (:file "server")))

(defpackage #:wimelib-archivist-config (:export #:*base-directory*))

(defparameter wimelib-archivist-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))
