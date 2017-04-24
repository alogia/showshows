(in-package :showshows)

(clsql:file-enable-sql-reader-syntax)

(setf clsql:*default-database-type* :sqlite3)


(defvar *database-path* "shows.db")

(defun init-database (path)
  "Basic init code to create a database if it does not exist."
  (clsql:connect (list path))
  (clsql:create-table [shows] '(([id]   integer :not-null :unique :primary-key :auto-increment)
				([name] string :not-null)
				([uri]  string :not-null)
				([img]  string) ; path to image in /images/
				([last] integer)
				([success] boolean))) ; all episodes found successfully when last attempted?
		      
  (clsql:create-table [episode] '(([id]   integer :not-null :unique :primary-key :auto-increment)
				  ([name] string :not-null)
				  ([season] string :not-null)
				  ([uri]  string :not-null)
				  ([last] integer) ;time last retreaved
				  ([have] integer))) ;link to [id] of [links]. nil means not downloaded.

  (clsql:create-table [links] '(([id]   integer :not-null :unique :primary-key :auto-increment)
				([episode] integer :not-null) ; link to [id] of [episode]
				([uri]  string :not-null)
				([last] integer) ; last time uri checked for validity
 				([success] boolean) ; last time link checked
				([size] integer)))) ; content-length from head

(defun add-show (shows)
  (loop for s in shows))

(defun add-season (season))

(defun add-episode (episode))
