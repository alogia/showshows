(in-package :showshows)

;;; Functions to parse the main Watchseries website


;; Functions for show listing page
;; ---------------------------------------------------------------

(defun get-seasons (node)
  "Call this method on the main seasons page for a show"
  (let* ((n-nodes (mapcar #'(lambda (n) (find-first-node n #'html-recurse-p (elements ("itemprop" . "name")))) (collect-nodes node (elements ("class" . "lists")))))
	 (names (mapcar #'(lambda (n) (html5-parser:node-value (html5-parser:node-first-child n))) n-nodes))
	 (listings (mapcar #'get-shows (collect-nodes node (elements ("class" . "listings show-listings"))))))
    (pairlis names listings)))
	
(defun process-show (node)
  "Internal function to parse show nodes"
  (let ((href (html5-parser:element-attribute node "href"))
	(name (html5-parser:node-value (html5-parser:node-first-child (find-first-node node #'html-recurse-p (elements ("itemprop" . "name"))))))
	(date (html5-parser:node-value (html5-parser:node-first-child (find-first-node node #'html-recurse-p (elements ("itemprop" . "datepublished")))))))
    (list :name name :date date :href href :node node)))  

(defun get-shows (node)
  "Internal function to parse season nodes"
  (mapcar (lambda (s) (process-show s))
	  (collect-nodes node (types "a"))))

;; Functions for host listing
;; --------------------------------------------------------------

(defun clean-host-links (links)
  "Returns only hosts which exist"
  (delete-if-not #'uri-exists? links))

(defun get-host-links (node)
  "Function to call to parse and decode all links on host listing page."
  (clean-host-links (mapcar #'decode-link (get-links (get-link-table node)))))

(defun get-link-table (node)
  "Called on show host list"
  (find-first-node node #'html-recurse-p (elements ("id" . "linktable"))))

(defun get-links (node)
  (cdr (mapcar (lambda (n) (html5-parser:element-attribute n "href"))
	  (collect-nodes node (elements ("target" . "_blank") ("class" . "buttonLink"))))))
