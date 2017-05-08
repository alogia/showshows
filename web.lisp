(in-package :showshows)
(require 'hunchentoot)
(require 'smackjack)
(require 'cl-who)

;;;; Set up and maintain hunchentoot server for the purpose of listing episodes.
;;;; Pulls all episode info from the clsql *database*

(defvar *web-server* nil)
(defvar *ajax-uri* "/request")
(defparameter *ajax-api* (make-instance 'smackjack:ajax-processor :server-uri *ajax-uri*))

(defmacro span (class string)
  `(cl-who:htm (:span :class ,class (cl-who:str ,string))))

(defmacro td (class string)
  `(cl-who:htm (:td :class ,class (cl-who:str ,string))))

(defmacro li (class string)
  `(cl-who:htm (:li :class ,class (cl-who:str ,string))))
  
(defmacro link-js (path)
  `(cl-who:htm
    (:script :type "text/javascript"
	     :src ,path)))

(defmacro link-css (path)
  `(cl-who:htm
    (:link :type "text/css"
	   :rel "stylesheet"
	   :href ,path)))

(defmacro html-out (body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
    ,body))

(defmacro html-out-p (body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    ,body))

(defmacro make-a-link (text link)
  `(cl-who:htm (:a
		:href ,link
		,text)))

(defmacro make-ajax-link (text link element)
  "Returns an a link named 'text' with a request to 'link' whose response will replace 'element'"
  `(cl-who:htm (:a
		:href "#"
		:onclick (format nil "$.get(\"~a\", function (data) { $(\"~a\").html(data); update();});" ,link ,element)
		,text)))

(defmacro make-js-link (text function args)
  "Returns an a link with onclick pointing at 'function' with arguments 'args'"
  `(cl-who:htm (:a
		:href "#"
		:onclick (format nil "~a(~{~a~^,~});" ,function ,args)
		,text)))

(defun make-get-link (callback vars &optional escaped?)
  "Takes a lisp function and converts it to an html get request link. vars are the variables to pass to args of the function or nil."
   (concatenate
    'string
    *ajax-uri* "/"
    (symbol-name callback) "?"
    (format nil "~{~A~^&~}"
	    (loop
	       for s in (sb-introspect:function-lambda-list callback) with x = 0
	       collect (concatenate
			'string
			(string-downcase (symbol-name s))
			(if escaped?
			    (format nil "=\\\"~a\\\"" (string (nth x vars)))
			    (format nil "=\"~a\"" (string (nth x vars)))))))))
    
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

(defun make-landing ()
  "Creates the landing page for hunchentoot."
  (hunchentoot:define-easy-handler (front-page :uri "/") ()
    (page-skel "Showshows" (:div :class "view"
				 (:div :class "episode-list"
				       (make-ajax-link
					"True Detective"
					(make-get-link 'show-info '("foo") t)
					".view"))))))

(defun generate-show-accordion (show)
  (with-slots (name url seasons) show
    (html-out
     (:div :class "show accordion"
	      (loop for se in seasons do
		   (generate-season-accordion se))))))

(defmacro generate-season-accordion (season)
  `(with-slots (show num episodes) ,season
      (cl-who:htm
	(:div (format t "Season ~D" num))
	(:div :class "season" (loop for ep in episodes do
	     (generate-episode-div ep)))))))

(defmacro generate-episode-div (episode)
  `(with-slots (show season num name date url) ,episode
    (cl-who:htm 
     (:ul :class "episode"
	   (li "number" num)
	   (li "name" name)
	   (li "date" date)))))
   
(defun generate-show-tables (show)
  (with-slots (name url seasons) show
    (html-out
	(:div :class "show"
	      (:meta :name "url" :content url)
	      (loop for se in seasons do
		   (generate-season-table se))))))

(defmacro generate-season-table (season)
  `(with-slots (show num episodes) ,season
      (cl-who:htm
	(:table :class "season"
	      (:meta :name "show" :content show)
	      (:tr (:th "#")
		   (:th "Name")
		   (:th "Date"))
	      (loop for ep in episodes do
		   (generate-episode-td ep))))))

(defmacro generate-episode-td (episode)
  `(with-slots (show season num name date url) ,episode
    (cl-who:htm
      (:tr :class "episode"
	   (td "number" num)
	   (td "name" name)
	   (td "date" date))))))

(defmacro page-skel (title &body body)
  "Defines the basic html code which should subsume page individualizations."
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
           (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                  (:title ,title)
		  (link-css "/web/css/default.css")
		  (link-css "/web/css/jquery-ui.css")
		  (link-js  "/web/javascript/jquery.min.js")
		  (link-js  "/web/javascript/jquery-ui.min.js")
		  (link-js  "/web/javascript/ajax.js"))
	   (:body ,@body))))

;; Definition of the basic ajax callback for requesting show info. 
(smackjack:defun-ajax show-info (show) (*ajax-api* :callback-data :response-text)
  (generate-show-accordion *show*))

;; Add the ajax functions into the hunchentoot disbatch table. 
(setq hunchentoot:*dispatch-table* (list 'hunchentoot:dispatch-easy-handlers
					 (smackjack:create-ajax-dispatcher *ajax-api*)))
