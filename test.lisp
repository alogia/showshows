(in-package :showshows)

(defvar *dom* nil)
(defvar *show* nil)

(defun load-test ()
  (setf *dom* (get-dom "http://onwatchseries.to/serie/true_detective"))
  (setf  *show* (parse-show "True Detective" "http://true-detective.com" *dom*)))
