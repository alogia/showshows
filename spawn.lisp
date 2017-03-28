(in-package :showshows)

(require 'bordeaux-threads)

;;; A basic thread manager to manage all web requests.
;;; The spawn manager will keep the number of threads to a
;;; given max number. Each thread calls the (spawn) method  
;;; and passes the spawnable object as argument. 


(defvar *spider-lock* (bt:make-lock)) ;;A file lock to access the returned values of objects
(defvar *spider-objects* '()) ; List of known objects added by (keep-spawnable)
(defvar *spawn-list* '()) ; *spawn-list* is the running list of objects waiting to spawn a thread
(defvar *spawn-lock* (bt:make-lock)) ; Lock to access global spawn manager variables
(defvar *spawn-threads* '()) ; Threads created by the spawn manager 
(defvar *spawn-manager* nil) ; The controlling running thread
(defvar *spawn-threads-run* 0) ; Total number of threads run while the *spawn-manager* thread is alive
(defvar *spawn-log* (pathname "spawn.log")) ; path to the spawn log file
(defvar *spawn-log-lock* (bt:make-lock)) ; lock for the spawn log file

;; Generic superclass for all objects that can be passed to the spawn manager.
;; Subclass and pass to (spawn-request)
(defclass spawnable ()
  ((url
    :initarg :url
    :initform (error "Error: No url provided")
    :reader url
    :documentation "The url of the page containing the video link")))

(defgeneric spawn (spawnable)
  (:documentation "Main method of spawnable called by the spawn-manager at spawn. Must return error code or 200 on success."))

(defun keep-spawnable (spawnable)
  "Adds a spawnable object to the list of known spawnable things."
  (push spawnable *spider-objects*))

(defmacro spawn-log (&rest text)
  "Logging macro for spawn-manager. Can be called by any spawnable objects to write to spawn.log"
  `(bt:with-lock-held (*spawn-log-lock*)
    (with-open-file (log *spawn-log* :direction :output :if-exists :append :if-does-not-exist :create)
      (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
	  (get-decoded-time)
	(declare (ignorable day-of-week dst-p))
	(format log "~2,'0d/~2,'0d/~2,'0d - ~d:~2,'0d:~d (GMT~@d):  "
		year month date hour minute second (- tz))
	(format log ,@text)
	(format log "~%")))

(defun spawn-request (spawnable)
  "Request that the given spawnable object and spawning function be added to the waiting list of spawning threads"
  (bt:with-lock-held (*spawn-lock*)
    (setf *spawn-list* (append *spawn-list* (list spawnable)))))

(defun manage-spawn (max-threads interval)
  "Starts a thread to manage the amount of spawning objects. interval is the sleep time between checks on still running threads. Terminates on *spawn-list* and *spawn-threads* == 0."
  (spawn-log "--- Starting to spawn ---")
  (spawn-log "Requests in queue: ~D" (list-length *spawn-list*))
  (setf *spawn-manager*
	(bt:make-thread (lambda ()
			  (loop
			     (let ((n 0))
			       (bt:with-lock-held (*spawn-lock*) (setf n (list-length (purge-finished))))
			       (if (<= n max-threads)
				   (if (not (= (bt:with-lock-held (*spawn-lock*) (list-length *spawn-list*)) 0))
				       (let ((th (bt:with-lock-held (*spawn-lock*) (pop *spawn-list*))))
					 (spawn-thread th)) ; Start a new thread of there is a request and under max-threads
				       (if (= n 0) ; if no threads running
					   (progn
					     (spawn-log "--- Spawn Manager finished ---")
					     (spawn-log "Threads run: ~D" *spawn-threads-run*)
					     (setf *spawn-threads-run* 0)
 					     (return)) ; Return when nothing to do
					   (sleep interval))) ; Sleep if there are still threads running, but nothing new to spawn
				   (sleep interval))))) ; Sleep if there are too many threads running. 
			:name "Spawn Manager")))

(defun shutdown-spawn ()
  "Shuts down the spawn manager"
  (spawn-log "--- Shutting down Spawn Manager ---")
  (spawn-log "Threads run: ~D" *spawn-threads-run*)
  (bt:destroy-thread *spawn-manager*)
  (setf *spawn-threads-run* 0))



(defun spawn-thread (spawnable)
  "Called by the spawn manager to initiate a new host thread. func takes one argument which is the cons object in the queue."
  (with-slots (url) spawnable
    (bt:with-lock-held (*spawn-lock*) (incf *spawn-threads-run*))
    (spawn-log "Spawning spider for ~a" url)
    (bt:with-lock-held (*spawn-lock*)
      (push (bt:make-thread (lambda ()
			      (handler-case
				  (let ((code (spawn spawnable)))
				    (spawn-log "Spider for ~a finished with code ~D" url code))
				#+sbcl(sb-int:simple-stream-error (se) (spawn-log "Whoops, ~a didn't work. ~a" url se))
				(DRAKMA::DRAKMA-SIMPLE-ERROR (se) (spawn-log "Error? ~a threw ~a" url se))
				(USOCKET:TIMEOUT-ERROR (se) (spawn-log "timeout error ~a threw ~a" url se))
				(USOCKET:NS-HOST-NOT-FOUND-ERROR (se) (spawn-log "host-not-found error ~a threw ~a" url se))
				(FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR (se) (spawn-log "~a threw ~a" url se)))) 
			    :name url)
	    *spawn-threads*))))

(defun purge-finished ()
  "Cleans up the threads list"
  (bt:with-lock-held (*spawn-lock*) (setf *spawn-threads* (delete-if-not #'bt:thread-alive-p *spawn-threads*))))

(defun spawn-select (predicate)
  "Filter function with test predicate around the list of spider objects"
  (mapcar #'spawn-request (remove-if-not predicate *spider-objects*)))

