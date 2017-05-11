(defsystem showshows
  :name "ShowShows"
  :version "0.1"
  :author "Tyler Thomas <alogia@gmail.com>"
  :description "Show streaming shows"
  :serial t
  :components ((:file "package")
	       (:file "main")
	       (:file "echos")
	       (:file "spawn")
	       (:file "show")
	       (:file "host")
	       (:file "watchseries")
	       (:file "vidto.me")
	       (:file "database")
	       (:file "web")
	       (:file "test"))
  :depends-on (:drakma
	       :cl-base64
	       :cl-ppcre
	       :cl-html5-parser
	       :clsql
	       :md5
	       :smackjack
	       :parenscript
	       :cl-json
	       :cl-who
	       :bordeaux-threads
	       :hunchentoot))
