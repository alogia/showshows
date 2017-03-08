(in-package :showshows)
(require 'hunchentoot)
(require 'cl-who)
;;;; Set up and maintain hunchentoot server for the purpose of listing episodes.
;;;; Pulls all episode info from the clsql *database*

(defvar *web-server* nil)

(defun get-show-info (show)
  (json:decode-json-from-string (map 'string 'code-char (drakma:http-request (make-get-request "http://www.omdbapi.com" `(("t" . ,show) ("r" . "json"))) :method :get))))

(defun get-show-poster (show)
  (cdr (assoc :*poster (get-show-info show))))

(defun init-web (port)
  "Basic init code to open a new instance of hunchentoot and create the landing page."
  (if (null *web-server*)
      (progn
	(setf *web-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :document-root #p"/home/phetus/devel/showshows/" :port port)))
	(make-landing))))


(defun make-landing ()
(hunchentoot:define-easy-handler (front-page :uri "/") ()
    (page-skel "Showshows" ((:img :src (get-show-poster "game of thrones"))))))

    
(defmacro page-skel (title &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
           (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                  (:title ,title)
		  (:link :type "text/css"
			 :rel "stylesheet"
			 :href "web/css/default.css"))
	   (:body ,@body))))
    

