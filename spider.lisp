(in-package :showshows)

(require 'bordeaux-threads)
(require 'cl-html5-parser)

;;; A basic thread handler to manage the web crawling of all listed
;;; sites. The spawn manager will keep the number of threads to a
;;; given max number. Each thread performs a web request to the given
;;; site, which will then fill in the host objects data structures. 

;;A file lock to access the returned values of host objects
(defvar *spider-lock* (bt:make-lock))

;;List of objects the spider knows about
(defvar *spider-objects* '())

;; *spawn-list* is the running list of objects waiting to spawn a thread
(defvar *spawn-list* '())
(defvar *spawn-lock* (bt:make-lock))
(defvar *spawn-threads* '())
(defvar *spawn-manager* nil)

(defun add-host (h)
  "Adds a host to the spider"
  (push h *spider-objects*))

(defun spawn-request (h)
  "Request that the given object be added to the waiting list of spawning threads"
  (bt:with-lock-held (*spawn-lock*)
    (append h *spawn-list*)))

(defun manage-spawn (max-threads interval)
  "Starts a thread to manage the amount of spawning objects. interval is the sleep time between checks on still running threads."
  (setf *spawn-manager*
	(bt:make-thread (lambda ()
			  (loop
			     (let ((n 0))
			       (bt:with-lock-held (*spawn-lock*) (setf n (list-length (purge-finished))))
			       (if (<= n max-threads)
				   (spawn-host-thread (bt:with-lock-held (*spawn-lock*) (pop *spawn-list*)))
				   (sleep interval))))))))

(defun kill-spawn-manager ()
  "Shuts down the spawn manager"
  (bt:destroy-thread *spawn-manager*))

(defun spawn-host-thread (h)
  "Called by the spawn manager to initiate a new host thread"
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
	*spawn-threads*))

(defun purge-finished ()
  "Cleans up the threads list"
  (delete-if-not #'bt:thread-alive-p *spawn-threads*))

(defun spawn-select (predicate)
  (mapcar #'spawn-host-thread (remove-if predicate *spider-objects*)))

