(in-package :showshows)

(defclass episode (spawnable)
  ((show
    :initarg :show
    :initform (error "Error: episode must be part of a show")
    :reader show
    :documentation "Name of the show to which this episode belongs")
   (season
    :initarg :season
    :initform (error "Error: episode must have a season")
    :reader season
    :documentation "Season number to which this episode belongs")
   (num
    :initarg :num
    :initform (error "Error: episode must have a number")
    :reader num
    :documentation "Number of this episode in the season")
   (name
    :initarg :name
    :reader name
    :documentation "Name of episode")
   (date
    :initarg :date
    :reader date
    :documentation "Date of episode")
   (url
    :initarg :url
    :reader url
    :documentation "Url of episode")
   (hosts
    :initform '()
    :accessor hosts
    :documentation "Hosts of episode")))

(defmethod spawn ((e episode))
  )

(defun create-season (show num episodes)
  "Acons list to represent a season"
  (list (cons :show show) (cons :num num) (cons :episodes episodes)))

(defun create-show (name url seasons)
  "Acons list to represent a show"
  (list (cons :name name) (cons :url url) (cons :seasons seasons)))
  
