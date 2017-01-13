(in-package :showshows)

(defun get-seasons (node)
  (let* (
	 (n-nodes (mapcar #'(lambda (n) (find-first-node n #'html-recurse-p (elements ("itemprop" . "name")))) (collect-nodes node (elements ("class" . "lists")))))
	 (names (mapcar #'(lambda (n) (html5-parser:node-value (html5-parser:node-first-child n))) n-nodes))
	 (listings (mapcar #'get-shows (collect-nodes node (elements ("class" . "listings show-listings"))))))
    (pairlis names listings)))
	
(defun process-show (node)
  (let ((href (html5-parser:element-attribute node "href"))
	(name (html5-parser:node-value (html5-parser:node-first-child (find-first-node node #'html-recurse-p (elements ("itemprop" . "name"))))))
	(date (html5-parser:node-value (html5-parser:node-first-child (find-first-node node #'html-recurse-p (elements ("itemprop" . "datepublished")))))))
    (list :name name :date date :href href :node node)))  

(defun get-shows (node)
  (mapcar #'(lambda (s) (process-show s)) (collect-nodes node (types "a"))))

(defun get-link-table (node)
  (find-first-node node #'html-recurse-p (elements ("id" . "linktable"))))

(defun get-links (node)
  (mapcar (lambda (n) (html5-parser:element-attribute n "href")) (collect-nodes node (elements ("target" . "_blank") ("class" . "buttonLink")))))
