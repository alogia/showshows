(defsystem showshows
  :name "ShowShows"
  :version "0.1"
  :author "Tyler Thomas <alogia@gmail.com>"
  :description "Show streaming shows"
  :serial t
  :components ((:file "package")
	       (:file "main")
	       (:file "watchseries")
	       (:file "vidto.me")
	       (:file "database")
	       (:file "web"))
  :depends-on (:drakma
	       :cl-base64
	       :cl-ppcre
	       :cl-html5-parser
	       :clsql
	       :cl-who
	       :hunchentoot))
