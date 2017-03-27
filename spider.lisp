(in-package :showshows)

(require 'bordeaux-threads)

;;; A basic thread handler to manage the web crawling of all listed
;;; sites. The spawn manager will keep the number of threads to a
;;; given max number. Each thread performs a web request to the given
;;; site, which will then fill in the host objects data structures. 

;;A file lock to access the returned values of objects
(defvar *spider-lock* (bt:make-lock))

;;List of objects the spider knows about
(defvar *spider-objects* '())

;; *spawn-list* is the running list of objects waiting to spawn a thread
(defvar *spawn-list* '())
(defvar *spawn-lock* (bt:make-lock))
(defvar *spawn-threads* '())
(defvar *spawn-manager* nil)
(defvar *spawn-threads-run* 0)

(defvar *spawn-log* "spawn.log")
(defvar *spawn-log-lock* (bt:make-lock))

(defun add-host (h)
  "Adds a host to the spider"
  (push h *spider-objects*))

(defmacro spawn-log (&rest text)
  `(bt:with-lock-held (*spawn-log-lock*)
    (with-open-file (log *spawn-log* :direction :output :if-exists :append :if-does-not-exist :create)
      (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
	  (get-decoded-time)
	(declare (ignorable day-of-week dst-p))
	(format log "~2,'0d/~2,'0d/~2,'0d - ~d:~2,'0d:~d (GMT~@d):  "
		year month date hour minute second (- tz))
	(format log ,@text)
	(format log "~%")))))
      
(defun spawn-request (url obj func)
  "Request that the given object and spawning function be added to the waiting list of spawning threads"
  (bt:with-lock-held (*spawn-lock*)
    (setf *spawn-list* (append *spawn-list* (list (list (cons :url url) (cons :obj obj) (cons :func func)))))))

(defun manage-spawn (max-threads interval)
  "Starts a thread to manage the amount of spawning objects. interval is the sleep time between checks on still running threads. Terminates on *spawn-list* and *spawn-threads* == 0."
  (spawn-log "--- Starting to spawn ---")
  (spawn-log "---~D requests in queue." (list-length *spawn-list*))
  (setf *spawn-manager*
	(bt:make-thread (lambda ()
			  (loop
			     (let ((n 0))
			       (bt:with-lock-held (*spawn-lock*) (setf n (list-length (purge-finished))))
			       (if (<= n max-threads)
				   (if (not (= (list-length *spawn-list*) 0))
				       (let ((th (bt:with-lock-held (*spawn-lock*) (pop *spawn-list*))))
					 (spawn-thread (cdr (assoc :url th)) (cdr (assoc :obj th)) (cdr (assoc :func th)))) ; Start a new thread of there is a request and under max-threads
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



(defun spawn-thread (url obj func)
  "Called by the spawn manager to initiate a new host thread. func takes one argument which is the cons object in the queue."
  (incf *spawn-threads-run*)
  (spawn-log "Spawning spider for ~a" url)
  (push (bt:make-thread (lambda ()
			  (handler-case
			      (let ((code (funcall func obj)))
				(spawn-log "Spider for ~a finished with code ~D" url code))
			    #+sbcl(sb-int:simple-stream-error (se) (spawn-log "Whoops, ~a didn't work. ~a" url se))
			    (DRAKMA::DRAKMA-SIMPLE-ERROR (se) (spawn-log "Error? ~a threw ~a" url se))
			    (USOCKET:TIMEOUT-ERROR (se) (spawn-log "timeout error ~a threw ~a" url se))
			    (USOCKET:NS-HOST-NOT-FOUND-ERROR (se) (spawn-log "host-not-found error ~a threw ~a" url se))
			    (FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR (se) (spawn-log "~a threw ~a" url se)))) 
			:name url)
	*spawn-threads*))

(defun purge-finished ()
  "Cleans up the threads list"
  (setf *spawn-threads* (delete-if-not #'bt:thread-alive-p *spawn-threads*)))

(defun spawn-select (predicate)
  "Filter function with test predicate around the list of spider objects"
  (mapcar #'spawn-request (remove-if-not predicate *spider-objects*)))
