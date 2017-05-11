(in-package :showshows)

;;; Basic generic definitions for adding hosting websites.
;;; All hosting sites should subclass host and intantiate methods:
;;;     wait-post
;;;     parse-video

;; Basic class definition for hosting website.
(clsql:def-view-class host (spawnable echos)
  ((id
    :reader id
    :initarg :id
    :type integer 
    :db-constraints (:not-null :auto-increment)
    :db-kind :key)
   (episode-id
    :reader episode-id
    :initarg :episode-id
    :type integer
    :documentation "id of the episode to which this refers.")
   (url
    :reader url
    :initarg :url
    :initform nil
    :type string
    :documentation "The url of the hosting page")
   (video
    :accessor video
    :initarg :video
    :initform nil
    :type string
    :documentation "The url of the video")
   (last-checked
    :accessor last-checked
    :initarg :last-checked
    :initform nil
    :type wall-time
    :documentation "Last time the uri was checked as valid.")
   (success
    :accessor success
    :initarg :success
    :initform nil
    :type bool
    :documentation "Was the last check successful.")
   (size
    :accessor size
    :initarg :size
    :initform nil
    :type integer
    :documentation "The size of the video.")))


(defgeneric wait-post (host)
  (:documentation "Post data to wait ad page on first call"))

(defgeneric parse-video (host dom)
  (:documentation "Parse the video url from returned html"))

(defmethod spawn ((h host))
  "Function to pass to the spawn manager when spawning a thread to handle spidering host websites. Errors caught in (spawn-host)"
  (let* ((res (wait-post h))
	 (v (parse-video h res))
	 (e (uri-exists? v)))
    (bt:with-lock-held (*spider-lock*)
      (progn (setf (video h) v)
	     (setf (last-checked h) (get-universal-time))
	     (setf (success h) e)
	     200)))) ;; success: return 200 

(defmethod echo ((h host)))
(defmethod echo-html ((h host)))
