(in-package :showshows)

(clsql:file-enable-sql-reader-syntax)

(setf clsql:*default-database-type* :sqlite3)


(defvar *database-path* "shows.db")

(defun init-database (path)
  "Basic init code to create a database if it does not exist."
  (clsql:disconnect)
  (clsql:connect (list path))
  (if (not (clsql:table-exists-p [show]))
      (clsql:create-view-from-class 'show))
  (if (not (clsql:table-exists-p [season]))
      (clsql:create-view-from-class 'season))
  (if (not (clsql:table-exists-p [episode]))
      (clsql:create-view-from-class 'episode)))
      

(defun add-show (shows)
  (loop for s in shows))

(defun add-season (season))

(defun add-episode (episode))
