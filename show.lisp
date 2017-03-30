(in-package :showshows)

(defclass episode (spawnable)
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


(defclass season (spawnable)
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

(defclass show (spawnable)
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
