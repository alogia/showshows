(in-package :showshows)

(require 'cl-who)

;;; Class definitions for show, season, and episodes information. 


;; Class containing information about show episodes, subclasses spawnable and echos
;; Contains list of hosts for episode
;; Must impliment spawn, echo, echo-html

(defclass episode (spawnable echos)
  ((show
    :initarg :show
    :initform (error "Error: episode must be part of a show")
    :reader show
    :documentation "Parent show to which this episode belongs")
   (season
    :initarg :season
    :initform (error "Error: episode must have a season")
    :reader season
    :documentation "Season to which this episode belongs")
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

(defmethod spawn ((ep episode))
  (with-slots (url hosts) ep
    (setf hosts (get-host-links (get-dom url)))))

(defmethod echo ((ep episode))
  ) ;;-------------------Fixme------------------

(defmethod echo-html ((ep episode))
  (with-slots (show season num name date url) ep
    (cl-who:with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
      (:div :class "episode" 
	    (:div :class "header"
		  (:meta :name "show" :content show)
		  (:meta :name "season" :content season)
		  (span "number" num)
		  (span "name" name)
		  (span "date" date))))))


;; Class containing information about show seasons, subclasses spawnable and echos
;; Contains list of episodes
;; Must impliment spawn, echo, echo-html

(defclass season (spawnable echos)
  ((show
    :initarg :show
    :initform (error "Error: episode must be part of a show")
    :reader show
    :documentation "Parent show to which this Season belongs")
   (num
    :initarg :num
    :initform (error "Error: Season must have a number")
    :reader num
    :documentation "Number of this season")
   (episodes
    :initarg :episodes
    :initform '()
    :accessor episodes
    :documentation "Episodes of season")))

(defmethod spawn ((se season))
  (with-slots (show url) se
    (setf se (process-season show (get-dom url)))))

(defmethod echo ((se season))
  ) ;;-------------------Fixme------------------

(defmethod echo-html ((se season))
  (with-slots (show num episodes) se
      (cl-who:with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
	(:div :class "season"
	      (:meta :name "show" :content show)
	      (loop for ep in episodes do
		   (cl-who:str (echo-html ep))))))) 


;; Class containing information about shows, subclasses spawnable and echos
;; Contains list of seasons
;; Must impliment spawn, echo, echo-html

(defclass show (spawnable echos)
    ((name
      :initarg :name
      :initform (error "Error: Show must have a name")
      :reader name
      :documentation "Show's name")
     (url
      :initarg :url
      :accessor url
      :documentation "The url of the show on watchseries")
     (seasons
      :initarg :seasons
      :initform '()
      :accessor seasons
      :documentation "A list of seasons of this show")))

(defmethod spawn ((sh show))
  (setf sh (parse-show (name sh) (url sh) (get-dom (url sh)))))

(defmethod echo ((sh show))
  ) ;;-------------------Fixme------------------

(defmethod echo-html ((sh show))
  (with-slots (name url seasons) sh
      (cl-who:with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
	(:div :class "show"
	      (:meta :name "url" :content url)
	      (loop for se in seasons do
		   (cl-who:str (echo-html se))))))) 
