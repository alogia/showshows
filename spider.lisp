(in-package :showshows)

(require 'bordeaux-threads)
(require 'cl-html5-parser)

;;; A basic thread handler to manage the web crawling of all listed sites

;;A file lock to access the returned values of web crawls
(defvar *spider-lock* (bt:make-lock))
(defvar *spider-threads* '())

(defvar *spider-objects* '())

(defun addhost (h)
  (push h *spider-objects*))

(defun spawn-host-thread (h)
  (push (bt:make-thread (lambda ()
			  (let* ((res (wait-post h))
				 (v (parse-video h res))
				 (e (uri-exists? v)))
			    (bt:with-lock-held (*spider-lock*)
			      (progn (setf (dom h) res)
				     (setf (video-url h) v)
				     (setf (checked h) (get-universal-time))
				     (setf (exists h) e)))))
			:name (url h))
	*spider-threads*))
