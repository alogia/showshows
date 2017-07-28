(in-package :showshows)

(require 'clsql)
(require 'cl-who)

;;; Class definitions for show, season, and episodes information. 


;; Class containing information about show episodes, subclasses spawnable and echos
;; Contains list of hosts for episode
;; Must impliment spawn, echo, echo-html


(clsql:def-view-class episode (spawnable echos)
  ((id
    :reader id
    :initarg :id
    :type integer 
    :db-constraints (:not-null :auto-increment)
    :db-kind :key)
   (show-id
    :initarg :show-id
    :initform (error "Error: episode must be part of a show")
    :reader show-id
    :type integer
    :documentation "Parent show to which this episode belongs")
   (season-id
    :initarg :season-id
    :initform (error "Error: episode must have a season")
    :reader season-id
    :type integer
    :documentation "Season to which this episode belongs")
   (num
    :initarg :num
    :initform (error "Error: episode must have a number")
    :reader num
    :type integer
    :documentation "Number of this episode in the season")
   (name
    :initarg :name
    :reader name
    :type string
    :documentation "Name of episode")
   (date
    :initarg :date
    :reader date
    :type string 
    :documentation "Date of episode")
   (last-checked
    :accessor last-checked
    :initarg :last-checked
    :initform 0
    :type integer
    :documentation "Last time the uri was checked as valid.")
   (success
    :accessor success
    :initarg :success
    :initform 0
    :type integer
    :documentation "Was the last check successful.")
   (hosts
    :accessor hosts
    :initarg :hosts
    :db-kind :join
    :db-info (:join-class host
			  :home-key id
			  :foreign-key episode-id
			  :set t))))

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

(clsql:def-view-class season (spawnable echos)
  ((id
    :reader id
    :initarg :id
    :type integer 
    :db-constraints (:not-null :auto-increment)
    :db-kind :key)
   (show-id
    :initarg :show-id
    :initform 0
    :reader show-id
    :type integer
    :documentation "Parent show to which this season belongs")
   (num
    :initarg :num
    :initform 0
    :reader num
    :type integer
    :documentation "Number of this season")
   (episodes
    :accessor episodes
    :documentation "Episodes of season"
    :db-kind :join
    :db-info (:join-class episode
			  :home-key id
			  :foreign-key season-id
			  :set t))))

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

(clsql:def-view-class show (spawnable echos)
  ((id
    :type integer
    :reader id
    :initarg :id
    :db-constraints (:not-null :auto-increment)
    :db-kind :key
    :documentation "Database unique key.")
   (name
    :type string 
    :initarg :name
    :initform nil
    :reader name
    :documentation "Show's name")
   (img
    :accessor img
    :initarg :img
    :initform nil
    :type string
    :documentation "Image associated with the show.")
   (last-checked
    :accessor last-checked
    :initarg :last-checked
    :initform 0
    :type integer
    :documentation "Last time the link was scraped.")
   (success
    :accessor success
    :initarg :success
    :initform 0
    :type integer
    :documenation "Was the last check successful.")
   (seasons
    :accessor seasons
    :db-kind :join
    :db-info (:join-class season
			  :home-key id
			  :foreign-key show-id
			  :set t))))

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
