(in-package :showshows)
(use-package (list :cl-who :hunchentoot))

;;;; Set up and maintain hunchentoot server for the purpose of listing episodes.
;;;; Pulls all episode info from the clsql *database*

(defvar *web-server* nil)

(defun init-web (port)
  "Basic init code to open a new instance of hunchentoot and create the landing page."
  (if (null *web-server*)
      (setf *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port port)))))


(defun make-landing ()
 (define-easy-handler (front-page :uri "/") ()
  
(defmacro page-skel ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en" :lang "en"
           (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                  (:title ,title)
		  (:link :type "text/css"
			 :rel "stylesheet"
			 :href "web/css/main.css"))
	   (:body ,@body))))
