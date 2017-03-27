(in-package :showshows)

;;; Basic generic definitions for adding hosting websites.
;;; All hosting sites should subclass host and intantiate methods:
;;;     wait-post
;;;     parse-video

;;Generic host class for all hosting websites 
(defclass host (spawnable)
  ((video
    :initarg :video
    :accessor video-url
    :documentation "The url of the video")
   (dom
    :initarg :dom
    :accessor dom
    :documentation "cl-html5-parser dom returned by parsing http response")
   (checked
    :initarg :checked
    :accessor checked
    :documentation "The last date this link was checked as existant.")
   (exists
    :initarg :exists
    :accessor exists
    :documentation "Is this link still valid?")
   (dled
    :initarg :dled
    :accessor dled
    :documentation "Is this video downloaded?")))

(defgeneric wait-post (host)
  (:documentation "Post data to wait ad page on first call"))

(defgeneric parse-video (host dom)
  (:documentation "Parse the video url from returned html"))

(defmethod  spawn (host)
  "Function to pass to the spawn manager when spawning a thread to handle spidering host websites. Errors caught in (spawn-host)"
  (let* ((res (wait-post host))
	 (v (parse-video host res))
	 (e (uri-exists? v)))
    (bt:with-lock-held (*spider-lock*)
      (progn (setf (dom host) res)
	     (setf (video-url host) v)
	     (setf (checked host) (get-universal-time))
	     (setf (exists host) e)
	     200)))) ;; success: return 200 
