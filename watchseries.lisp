(in-package :showshows)

;;; Functions to parse the main Watchseries website


;; Functions for show listing page
;; ---------------------------------------------------------------

(defun init-show (name url)
  (parse-show name url (get-dom url)))
  
(defun parse-show (name url node)
  (make-instance 'show
		 :name name
		 :url url
		 :seasons (parse-seasons name node))))

(defun parse-seasons (name node)
  "Call this method on the main seasons page for a show"
    (reverse (mapcar #'(lambda (n) (process-season name n))
	    (collect-nodes node (elements ("itemprop" . "season"))))))

(defun process-season (show node)
  (let* ((listing (find-first-node node 'html-recurse-p (elements ("class" . "listings show-listings"))))
	 (num (parse-integer (car (ppcre:all-matches-as-strings "[0-9]+$" (html5-parser:element-attribute listing "id")))))
	 (a (find-first-node node 'html-recurse-p (types "a")))
	 (url (html5-parser:element-attribute a "href")))
    (make-instance 'season
		   :url url
		   :show show
		   :num num
		   :episodes (parse-episodes show num node))))

(defun parse-episodes (show season node)
  "Internal function to parse season nodes"
  (reverse (mapcar (lambda (s) (process-episode show season s))
	  (collect-nodes node (elements ("itemprop" . "episode"))))))

(defun process-episode (show season node)
  "Internal function to parse show nodes"
  (let ((num (html5-parser:element-attribute (find-first-node node #'html-recurse-p (elements ("itemprop" . "episodenumber"))) "content"))
	(a (find-first-node node #'html-recurse-p (types "a"))))
    (let
	 ((url (html5-parser:element-attribute a "href"))
	  (name (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "name")))))
	  (date (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "datepublished"))))))
      (make-instance 'episode
		     :show show
		     :season season
		     :num num
		     :name (if name (html5-parser:node-value name) "Unknown")
		     :date (if date (html5-parser:node-value date) "Unknown")
		     :url url)))) ; Init host links to nil, and fill only upon request.



;;;;;;;;;;;;;=============================================================================FIXME=========================================

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
