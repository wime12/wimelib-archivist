(in-package #:wimelib-archivist)

(define-handlers
    (new-address :uri "/archivist/new-address")
    (add-address :uri "/archivist/add-address") 
  "Add a new address"
  address
  ("Line 1 (optional)" line-1-extra :line-1-extra)
  ("Street and number" line-2 :line-2-street-and-number)
  ("Line 3 (optional)" line-3-extra :line-3-extra)
  ("Postal code" postal-code :postal-code)
  ("City" city :city)
  ("State or province" state-or-province :state-or-province)
  ("Country (optional)" country :country)
  ("Other details" other-address-details :other-address-details))

(define-handlers
    (new-sheet-music :uri "/archivist/new-sheet-music")
    (add-sheet-music :uri "/archivist/add-sheet-music")
  "Add new sheet music"
  sheet-music
  ("Title" music-title :music-title)
  ("Data of publication" data-of-publication :data-of-publication)
  ("ISBN" isbn :isbn)
  ("Description" music-description :music-description)
  ("Voices" voices :voices)
  ("Other details" other-sheet-music-details :other-sheet-music-details))

(define-handlers
    (new-composer :uri "/archivist/new-composer")
    (add-composer :uri "/archivist/add-composer")
  "Add a composer"
  composer
  ("First name" first-name :first-name)
  ("Last name" last-name :last-name)
  ("Other details" other-composer-details :other-composer-details))

(define-handlers
    (new-choir-member :uri "/archivist/new-choir-member")
    (add-choir-member :uri "/archivist/add-choir-member")
  "Add a choir member"
  choir-member
  ("Gender" gender :gender)
  ("First name" first-name :first-name)
  ("Last name" last-name :last-name)
  ("Date of birth" date-of-birth :date-of-birth)
  ("E-Mail" e-mail-address :e-mail-address)
  ("Phone number" phone-number :phone-number)
  ("Cell phone number" cell-number :cell-number)
  ("Other details" other-member-details :other-member-details))
