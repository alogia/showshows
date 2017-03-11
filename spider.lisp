(in-package :showshows)
(require 'bordeaux-threads)

;;A file lock to access the returned values of web crawls
(defvar *spider-lock* (bt:make-lock))

