(in-package :showshows)

;;; Basic generic definitions for adding hosting websites.
;;; All hosting sites should subclass host and intantiate methods:
;;;     wait-post
;;;     parse-video

;;Generic host class for all hosting websites 
(defclass host ()
  ((url
    :initarg :url
    :initform (error "Error: No url provided")
    :reader url
    :documentation "The url of the page containing the video link")
   (video
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

