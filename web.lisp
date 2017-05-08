(in-package :showshows)
(require 'hunchentoot)
(require 'smackjack)
(require 'cl-who)

;;;; Set up and maintain hunchentoot server for the purpose of listing episodes.
;;;; Pulls all episode info from the clsql *database*

(defvar *web-server* nil)
(defvar *ajax-uri* "/request")
(defparameter *ajax-api* (make-instance 'smackjack:ajax-processor :server-uri *ajax-uri*))
  
(defun get-show-info (show)
  "Pulls show info from the api on omdbapi.com and returns the data as a decoded json list"
  (json:decode-json-from-string (drakma:http-request (make-get-request "http://www.omdbapi.com" `(("t" . ,show) ("r" . "json"))) :method :get)))

(defun get-show-poster (show)
  "Pulls the poster from the show info retrieved in (get-show-info)"
  (cdr (assoc :*poster (get-show-info show))))

(defun init-web (port)
  "Basic init code to open a new instance of hunchentoot and create the landing page."
  (if (null *web-server*)
      (progn
	(setf *web-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :document-root #p"/home/phetus/devel/showshows/" :port port)))
	(make-landing))))

(defmacro to-html (body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
    ,@body))

(defmacro to-html-p (body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    ,@body))

(defun make-landing ()
  "Creates the landing page for hunchentoot."
  (hunchentoot:define-easy-handler (front-page :uri "/") ()
    (page-skel "Showshows" (make-get-link "True Detective" show-info ("foo")))))

(defun generate-show-tables (show)
  (with-slots (name url seasons) show
    (to-html
	(:div :class "show"
	      (:meta :name "url" :content url)
	      (loop for se in seasons do
		   (cl-who:str (generate-season-table se)))))))

(defun generate-season-table (season)
  (with-slots (show num episodes) season
      (cl-who:with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
	(:table :class "season"
	      (:meta :name "show" :content show)
	      (:tr (:th "#")
		   (:th "Name")
		   (:th "Date"))
	      (loop for ep in episodes do
		   (cl-who:str (generate-episode-td ep))))))) 

(defun generate-episode-td (episode)
  (with-slots (show season num name date url) episode
    (cl-who:htm
      (:tr :class "episode"
	   (td "number" num)
	   (td "name" name)
	   (td "date" date)))))

(defun make-table (table)
  "Pass this macro a list of lists to generate a table"
  (let ((tbl (make-column table)))
    `(:table ,@tbl)))

(defun make-column (columns)
  (loop for r in columns
	      collect `(:tr ,@(make-row r))))
		  
(defun make-row (rows)
  (loop for h in rows
       collect `(:th ,h)))

(defmacro page-skel (title &body body)
  "Defines the basic html code which should subsume page individualizations."
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
           (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                  (:title ,title)
		  (:link :type "text/css"
			 :rel "stylesheet"
			 :href "/web/css/default.css")
		  (:link :type "text/css"
			 :rel "stylesheet"
			 :href "/web/css/jquery-ui.css")
		  (:script :type "text/javascript"
			   :src "/web/javascript/jquery.min.js")
		  (:script :type "text/javascript"
			   :src "/web/javascript/jquery-ui.min.js")
		  (:script :type "text/javascript"
			   :src "/web/javascript/ajax.js"))
	   (:body ,@body))))

;; Definition of the basic ajax callback for requesting show info. 
(smackjack:defun-ajax show-info (show) (*ajax-api* :callback-data :response-text)
  (generate-show-tables *show*))

;; Add the ajax functions into the hunchentoot disbatch table. 
(setq hunchentoot:*dispatch-table* (list 'hunchentoot:dispatch-easy-handlers
					 (smackjack:create-ajax-dispatcher *ajax-api*)))


(defmacro make-get-link (text callback vars)
  "Takes a lisp function and converts it to an html get request link. vars are the variables to pass to args of the function or nil."
  (let ((ref (concatenate
	      'string
	      *ajax-uri* "/"
	      (symbol-name callback) "?"
	      (format nil "~{~A~^&~}"
		      (loop
			 for s in (sb-introspect:function-lambda-list callback) with x = 0
			 collect (concatenate
				  'string
				  (string-downcase (symbol-name s))
				  "=\"" (string (nth x vars)) "\""))))))
    `(cl-who:htm (:a
		  :href ,ref
		  ,text))))
