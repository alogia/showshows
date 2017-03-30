(in-package :showshows)

;;; Functions to parse the main Watchseries website


;; Functions for show listing page
;; ---------------------------------------------------------------

(defun get-seasons (name url node)
  "Call this method on the main seasons page for a show"
    (create-show name url (mapcar #'(lambda (n) (process-season name n))
		      (collect-nodes node (elements ("class" . "listings show-listings"))))))

(defun process-season (show node)
  (let ((num (parse-integer (car (ppcre:all-matches-as-strings "[0-9]+$" (html5-parser:element-attribute node "id"))))))
    (create-season show num (get-shows show num node))))

(defun get-shows (show season node)
  "Internal function to parse season nodes"
  (mapcar (lambda (s) (process-show show season s))
	  (collect-nodes node (elements ("itemprop" . "episode")))))

(defun process-show (show season node)
  "Internal function to parse show nodes"
  (let ((num (html5-parser:element-attribute (find-first-node node #'html-recurse-p (elements ("itemprop" . "episodenumber"))) "content"))
	(a (find-first-node node #'html-recurse-p (types "a"))))
    (let
	 ((href (html5-parser:element-attribute a "href"))
	  (name (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "name")))))
	  (date (html5-parser:node-first-child (find-first-node a #'html-recurse-p (elements ("itemprop" . "datepublished"))))))
      (create-episode show season num
		      (if name (html5-parser:node-value name) "Unknown")
		      (if date (html5-parser:node-value date) "Unknown")
		      href
		      nil)))) ; Init host links to nil, and fill only upon request.



;;;;;;;;;;;;;=============================================================================FIXME=========================================
(defun request-episode-hosts (episode)
  "Put in a request with the spawn manager to spider the episode listing pages on watchseries and return the host links."
  episode)

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
