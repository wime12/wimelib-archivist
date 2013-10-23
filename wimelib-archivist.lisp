;;;; wimelib-archivist.lisp

(in-package #:wimelib-archivist)

(defvar *database* (merge-pathnames "music-archive.db"
				    wimelib-archivist-config:*BASE-DIRECTORY*))

;; Database Schemata (adapted from www.databaseanswers.org/data_models)

(defclass sheet-music ()
  ((sheet-music-id :accessor sheet-music-id :initarg :sheet-music-id
		   :column-type :integer)
   (isbn :accessor isbn :initarg :isbn)
   (date-of-publication :accessor date-of-publication
			:initarg :date-of-publication)
   (music-title :accessor score-title :initarg :score-title)
   (music-description :accessor music-description :initarg :music-description)
   
   (voices :accessor voices :initarg :voices)
   (other-sheet-music-details :accessor sheet-music-other-details
			      :initarg :sheet-music-other-details))
  (:primary-key sheet-music-id)
  (:table-name sheet-music)
  (:metaclass da-class))

(defclass composer ()
  ((composer-id :accessor composer-id :initarg :composer-id
		:column-type :integer)
   (first-name :accessor first-name :initarg :first-name)
   (last-name :accessor last-name :initarg :last-name)
   (other-composer-details :accessor other-composer-details
			   :initarg :other-composer-details))
  (:primary-key composer-id)
  (:table-name composers)
  (:metaclass da-class))

(defclass music-by-composer ()
  ((composer-id :accessor composer-id :initarg :composer-id
		:not-null t :column-type :integer)
   (sheet-music-id :accessor sheet-music-id :initarg :sheet-music-id
		   :not-null t :column-type :integer))
  (:primary-key composer-id sheet-music-id)
  (:foreign-keys (composer-id sheet-music-id) sheet-music)
  (:metaclass da-class))

(defclass member ()
  ((member-id :accessor member-id :initarg :member-id
	      :column-type :integer)
   (member-address-id :accessor member-address-id
		      :initarg :member-address-id
		      :column-type :integer)
   (gender :accessor gender :initarg :gender)
   (member-first-name :accessor first-name :initarg :member-first-name)
   (member-last-name :accessor surename :initarg :member-last-name)
   (date-of-birth :accessor date-of-birth :initarg :date-of-birth)
   (e-mail-address :accessor e-mail-address :initarg :e-mail-address)
   (phone-number :accessor phone-number :initarg :phone-number)
   (cell-number :accessor cell-number :initarg :cell-number)
   (other-member-details :accessor other-member-details
			 :initarg :other-member-details))
  (:primary-key member-id)
  (:foreign-keys member-address-id (address address-id))
  (:table-name members)
  (:metaclass da-class))

(defclass address ()
  ((address-id :accessor address-id :initarg :address-id
	       :column-type :integer)
   (line-1-extra :accessor line-1-extra :initarg :line-1-extra)
   (line-2-street-and-number :accessor line-2-street-and-number
			     :initarg :line-2-street-and-number)
   (line-3-extra :accessor line-3-extra :accessor line-3-extra
		 :initarg :line-3-extra)
   (postal-code :accessor postal-code :initarg :postal-code)
   (state-or-province :accessor state-or-province :initarg :state-or-province)
   (country :accessor country :initarg :country)
   (other-address-details :accessor other-address-details
			  :initarg :other-address-details))
  (:primary-key address-id)
  (:table-name addresses)
  (:metaclass da-class))

(defclass copy ()
  ((sheet-music-id :accessor score-id :initarg :score-id
		   :column-type :integer :column-name sheet-music-id)
   (copy-number :accessor copy-id :initarg :copy-id
		:column-type :integer :column-name copy-number))
  (:primary-key sheet-music-id copy-number)
  (:foreign-keys sheet-music-id sheet-music)
  (:table-name copies)
  (:metaclass da-class))

;;; TODO: Constrain sheet-music-id/copy-number
(defclass issued-copy ()
  ((member-id :accessor member-id :initarg :member-id)
   (datetime-issued :accessor datetime-issued :initarg :datetime-issued)
   (datetime-returned :accessor datetime-returned :initarg :datetime-returned)
   (sheet-music-id :accessor sheet-music-id :initarg :sheet-music-id)
   (copy-number :accessor copy-number :initarg :copy-number))
  (:primary-key member-id sheet-music-id datetime-issued)
  (:foreign-keys member-id member)
  (:table-name issued-copies)
  (:metaclass da-class))
