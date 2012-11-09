;;;; wimelib-archivist.lisp

(in-package #:wimelib-archivist)

;; Database Schemata (adapted from www.databaseanswers.org/data_models)

(defclass sheet-music ()
  ((sheet-music-id :accessor sheet-music-id
		   :initarg :sheet-music-id
		   :column-type :integer
		   :initform :null)
   (isbn :accessor isbn
	 :initarg :isbn
	 :initform "")
   (date-of-publication :accessor date-of-publication 
			:initarg :date-of-publication
			:initform "")
   (music-title :accessor music-title
		:initarg :music-title
		:initform "")
   (music-description :accessor music-description
		      :initarg :music-description
		      :initform "")
   
   (voices :accessor voices
	   :initarg :voices
	   :initform "")
   (other-sheet-music-details :accessor other-sheet-music-details
			      :initarg :other-sheet-music-details
			      :initform ""))
  (:primary-key sheet-music-id)
  (:table-name sheet-music)
  (:metaclass da-class))

(defclass composer ()
  ((composer-id :accessor composer-id
		:initarg :composer-id
		:column-type :integer
		:initform :null)
   (first-name :accessor first-name
	       :initarg :first-name
	       :initform "")
   (last-name :accessor last-name
	      :initarg :last-name
	      :initform "")
   (other-composer-details :accessor other-composer-details
			   :initarg :other-composer-details
			   :initform ""))
  (:primary-key composer-id)
  (:table-name composers)
  (:metaclass da-class))

(defclass music-by-composer ()
  ((composer-id :accessor composer-id
		:initarg :composer-id
		:column-type :integer)
   (sheet-music-id :accessor sheet-music-id
		   :initarg :sheet-music-id
		   :column-type :integer))
  (:primary-key composer-id sheet-music-id)
  (:foreign-keys composer-id composer
		 sheet-music-id sheet-music)
  (:metaclass da-class))

(defclass choir-member ()
  ((member-id :accessor member-id
	      :initarg :member-id
	      :column-type :integer
	      :initform :null)
   (member-address-id :accessor member-address-id
		      :initarg :member-address-id
		      :column-type :integer)
   (gender :accessor gender
	   :initarg :gender
	   :initform "")
   (member-first-name :accessor first-name
		      :initarg :member-first-name
		      :initform "")
   (member-last-name :accessor surename
		     :initarg :member-last-name
		     :initform "")
   (date-of-birth :accessor date-of-birth
		  :initarg :date-of-birth
		  :initform "")
   (e-mail-address :accessor e-mail-address
		   :initarg :e-mail-address
		   :initform "")
   (phone-number :accessor phone-number
		 :initarg :phone-number
		 :initform "")
   (cell-number :accessor cell-number
		:initarg :cell-number
		:initform "")
   (other-member-details :accessor other-member-details
			 :initarg :other-member-details
			 :initform ""))
  (:primary-key member-id)
  (:foreign-keys member-address-id (address address-id))
  (:table-name members)
  (:metaclass da-class))

(defclass address ()
  ((address-id :accessor address-id
	       :initarg :address-id
	       :column-type :integer
	       :initform :null)
   (line-1-extra :accessor line-1-extra
		 :initarg :line-1-extra
		 :initform "")
   (line-2-street-and-number :accessor line-2-street-and-number
			     :initarg :line-2-street-and-number
			     :initform "")
   (line-3-extra :accessor line-3-extra :accessor line-3-extra
		 :initarg :line-3-extra
		 :initform "")
   (postal-code :accessor postal-code
		:initarg :postal-code
		:initform "")
   (city :accessor city
	 :initarg :city
	 :initform "")
   (state-or-province :accessor state-or-province
		      :initarg :state-or-province
		      :initform "")
   (country :accessor country
	    :initarg :country
	    :initform "")
   (other-address-details :accessor other-address-details
			  :initarg :other-address-details
			  :initform ""))
  (:primary-key address-id)
  (:table-name addresses)
  (:metaclass da-class))

(defclass copy ()
  ((sheet-music-id :accessor score-id
		   :initarg :score-id
		   :column-type :integer)
   (copy-number :accessor copy-id
		:initarg :copy-id
		:column-type :integer))
  (:primary-key sheet-music-id copy-number)
  (:foreign-keys sheet-music-id sheet-music)
  (:table-name copies)
  (:metaclass da-class))

;;; TODO: Constrain sheet-music-id/copy-number
(defclass issued-copy ()
  ((member-id :accessor member-id
	      :initarg :member-id)
   (datetime-issued :accessor datetime-issued
		    :initarg :datetime-issued
		    :initform "")
   (datetime-returned :accessor datetime-returned
		      :initarg :datetime-returned
		      :initform "")
   (sheet-music-id :accessor sheet-music-id
		   :initarg :sheet-music-id
		   :initform "")
   (copy-number :accessor copy-number
		:initarg :copy-number
		:initform ""))
  (:primary-key member-id sheet-music-id datetime-issued)
  (:foreign-keys member-id choir-member
		 sheet-music-id sheet-music)
  (:table-name issued-copies)
  (:metaclass da-class))

(defvar *tables* '(sheet-music composer music-by-composer address choir-member copy
		   issued-copy))

(defun create-table (table)
  (exec* (da-class-schema table)))

(defun create-tables (tables)
  (with-transaction
    (mapc #'create-table tables)))

(defun delete-table (table)
  (exec (:drop :table @(da-class-table-name (find-class table)))))

(defun delete-tables (tables)
  (with-transaction
    (mapc #'delete-table tables)))

(defmacro with-foreign-keys (&body body)
  `(progn
     (exec (:pragma :foreign-keys := :on))
     ,@body))
