(in-package :showshows)

(clsql:file-enable-sql-reader-syntax)

(setf clsql:*default-database-type* :sqlite3)


(defvar *database-path* "shows.db")

(defun init-database (path)
  "Basic init code to create a database if it does not exist."
  (clsql:connect (list path)))

(defun add-show (shows)
  (loop for s in shows))

(defun add-season (season))

(defun add-episode (episode))
