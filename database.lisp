(in-package :showshows)
(clsql:file-enable-sql-reader-syntax)

(defvar *database-path* "shows.db")
(setf clsql:*default-database-type* :sqlite3)


(defun init-database (path)
  "Basic init code to create a database if it does not exist."
  (clsql:connect (list path))
  (clsql:create-table [shows] '(([id]   integer)
				([name] string :not-null)
				([uri]  string :not-null)
				([last] integer)
				([success] boolean)) ; all episodes found successfully when last attempted?
		      :constraints '("UNIQUE (id)" "PRIMARY KEY (id)"))

  (clsql:create-table [episode] '(([id]   integer)
				  ([name] string :not-null)
				  ([season] string :not-null)
				  ([uri]  string :not-null)
				  ([last] integer) ;time last retreaved
				  ([have] integer)) ;link to [id] of [links]. nil means not downloaded.
		      :constraints '("UNIQUE (id)" "PRIMARY KEY (id)"))

  (clsql:create-table [links] '(([id]   integer)
				([episode] integer :not-null)
				([name] string :not-null)
				([uri]  string :not-null)
				([last] integer) ; last time uri checked for validity
				([success] boolean) ; last time link checked
				([size] integer)) ; content-length from head
		      :constraints '("UNIQUE (id)" "PRIMARY KEY (id)")))
