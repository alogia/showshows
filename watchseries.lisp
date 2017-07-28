(in-package :showshows)
(clsql:file-enable-sql-reader-syntax)

;;; Functions to parse the main Watchseries website


;; Functions for show listing page
;; ---------------------------------------------------------------


(defun init-show (name url)
  (parse-show name url (get-dom url)))

(defun parse-show (name url node)
  (let ((show-id (caar (clsql:select [id] :from [show] :where [= [url] url]))))
    (if show-id
	(progn (parse-seasons node show-id)
	       (caar (clsql:select 'show :from [show] :where [= [id] show-id])))
	(let ((show-inst (make-instance 'show :name name :url url)))
	  (parse-seasons node (car (clsql:update-records-from-instance show-inst)))
	   show-inst))))

(defun parse-seasons (node show-id)
  "Call this method on the main seasons page for a show"
    (mapcar #'(lambda (n) (process-season n show-id))
	    (collect-nodes node (elements ("itemprop" . "season")))))

(defun process-season (node show-id)
  "Internal function to parse the season nodes"
  (let* ((listing (find-first-node node 'html-recurse-p (elements ("class" . "listings show-listings"))))
	 (num (parse-integer (car (ppcre:all-matches-as-strings "[0-9]+$" (html5-parser:element-attribute listing "id")))))
	 (a (find-first-node node 'html-recurse-p (types "a")))
	 (url (html5-parser:element-attribute a "href")))
    (let ((season-id (caar (clsql:select [id] :from [season] :where [= [num] num] :and [= [show-id] show-id]))))
      (if season-id
	  (progn
	    (parse-episodes node show-id season-id);
	    (caar (clsql:select 'season :from [season] :where [= [id] season-id])))
	  (let ((season-inst (make-instance 'season
					       :url url
					       :show-id show-id
					       :num num)))
	    (parse-episodes node show-id (car (clsql:update-records-from-instance season-inst)))
	    season-inst)))))

(defun parse-episodes (node show-id season-id)
  "Internal function to parse season nodes"
  (let ((new-eps (mapcar (lambda (s) (process-episode s show-id season-id))
	  (collect-nodes node (elements ("itemprop" . "episode"))))))
    (dolist (e new-eps)
      (if e
	  (progn
	    (clsql:update-records-from-instance e)
	    new-eps)))))

(defun process-episode (node show-id season-id)
  "Internal function to parse episode nodes"
  (let ((num (html5-parser:element-attribute (find-first-node node #'html-recurse-p (elements ("itemprop" . "episodenumber"))) "content"))
	(a (find-first-node node #'html-recurse-p (types "a"))))
    (let*
	 ((url (html5-parser:element-attribute a "href"))
	  (name (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "name")))))
	  (date (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "datepublished")))))
	  (ep-url (caar (clsql:select [url] :from [episode] :where [= [url] url] :and [= [season-id] season-id]))))
      (if (not (equalp url ep-url))
	  (make-instance 'episode
			 :show-id show-id
			 :season-id season-id
			 :num (parse-integer num)
			 :name (if name (html5-parser:node-value name) "Unknown")
			 :date (if date (html5-parser:node-value date) "Unknown")
			 :url url))))) ; Init host links to nil, and fill only upon request.


;; Functions for host listing
;; --------------------------------------------------------------

(defun get-host-links (node)
  "Function to call to parse and decode all links on host listing page."
  (clean-host-links (mapcar #'decode-link (get-links (get-link-table node)))))

(defun clean-host-links (links)
  "Returns only hosts which exist"
  (delete-if (lambda (l) (or (equal nil l) (not (uri-exists? l)))) links))

(defun get-link-table (node)
  "Called on show host list"
  (find-first-node node #'html-recurse-p (elements ("id" . "linktable"))))

(defun get-links (node)
  (cdr (mapcar (lambda (n) (html5-parser:element-attribute n "href"))
	  (collect-nodes node (elements ("target" . "_blank") ("class" . "buttonLink"))))))
