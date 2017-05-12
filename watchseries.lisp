(in-package :showshows)

;;; Functions to parse the main Watchseries website


;; Functions for show listing page
;; ---------------------------------------------------------------

(defun init-show (name url)
  (parse-show name url (get-dom url)))

(defun parse-show (name url node)
  (let ((show-id (car (clsql:update-records-from-instance (make-instance 'show :name name :url url)))))
	(parse-seasons node show-id)))

(defun parse-seasons (node show-id)
  "Call this method on the main seasons page for a show"
    (mapcar #'(lambda (n) (clsql:update-records-from-instance (process-season n show-id)))
	    (collect-nodes node (elements ("itemprop" . "season"))))))

(defun process-season (node show-id)
  "Internal function to parse the season nodes"
  (let* ((listing (find-first-node node 'html-recurse-p (elements ("class" . "listings show-listings"))))
	 (num (parse-integer (car (ppcre:all-matches-as-strings "[0-9]+$" (html5-parser:element-attribute listing "id")))))
	 (a (find-first-node node 'html-recurse-p (types "a")))
	 (url (html5-parser:element-attribute a "href")))
    (let ((season-id (make-instance 'season
		   :url url
		   :show-id show-id
		   :num num)))
    (parse-episodes node show-id season-id))))

(defun parse-episodes (node show-id season-id)
  "Internal function to parse season nodes"
  (mapcar (lambda (s) (clsql:update-records-from-instance (process-episode s show-id season-id)))
	  (collect-nodes node (elements ("itemprop" . "episode")))))

(defun process-episode (node show-id season-id)
  "Internal function to parse episode nodes"
  (let ((num (html5-parser:element-attribute (find-first-node node #'html-recurse-p (elements ("itemprop" . "episodenumber"))) "content"))
	(a (find-first-node node #'html-recurse-p (types "a"))))
    (let
	 ((url (html5-parser:element-attribute a "href"))
	  (name (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "name")))))
	  (date (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "datepublished"))))))
      (make-instance 'episode
		     :show-id show-id
		     :season-id season-id
		     :num num
		     :name (if name (html5-parser:node-value name) "Unknown")
		     :date (if date (html5-parser:node-value date) "Unknown")
		     :url url)))) ; Init host links to nil, and fill only upon request.


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
2
