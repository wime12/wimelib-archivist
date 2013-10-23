(in-package #:wimelib-archivist)

(defmacro with-html-output-to-string ((string-stream) &body body)
  `(with-output-to-string (,string-stream)
     (with-html-output (,string-stream)
       ,@body)))

(defmacro define-handlers (view-handler controler-handler title class-name
			   &rest fields)
  `(progn
     (define-easy-handler ,view-handler ()
       (with-html-output-to-string (str)
	 (:title ,title)
	 (:form :action ,(handler-uri controler-handler)
		:method "post"
		,@(generate-fields fields)
		(:p (:input :type "submit"
			    :value "Add"
			    :class "btn")))))
     (define-easy-handler ,controler-handler
	 ,(generate-variables fields)
       (with-html-output-to-string (str)
	 (with-db *database*
	   (insert-da
	    (make-instance ',class-name
			   ,@(generate-initialization-arguments fields))))))))

(defun handler-uri (handler-declaration)
  (destructuring-bind (fn-name &key uri) handler-declaration
    (declare (ignore fn-name))
    uri))

(defun generate-fields (field-declarations)
  (mapcar #'generate-field field-declarations))

(defun generate-field (field-declaration)
  (destructuring-bind (label name initarg
			     &key (type "text") (class "txt") (default ""))
      field-declaration
    (declare (ignore default initarg default))
    `(:p ,label (:input :type ,type :name ,(symbol-name name) :class ,class))))

(defun generate-variables (field-declarations)
  (mapcar #'generate-variable field-declarations))

(defun generate-variable (field-declaration)
  (destructuring-bind (label name initarg
			     &key (type "text") (class "txt") (default ""))
      field-declaration
    (declare (ignore label type class initarg))
    `(,name :init-form ,default)))

(defun generate-initialization-arguments (field-declarations)
  (mapcan #'generate-initialization-argument field-declarations))

(defun generate-initialization-argument (field-declaration)
  (destructuring-bind (label name initarg
			     &key (type "text") (class "txt") (default ""))
      field-declaration
    (declare (ignore label type class default))
    `(,initarg ,name)))
