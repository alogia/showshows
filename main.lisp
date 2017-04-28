(require :drakma)
(require :cl-ppcre)
(require :cl-base64)
(require :cl-html5-parser)
(in-package :showshows)

(defvar *user-agent* "Mozilla/5.0 (Linux; Android 5.1.1; Nexus 5 Build/LMY48B; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/43.0.2357.65 Mobile Safari/537.36")

(defvar *test* "http://vidto.me/rgvyvjj5nrt8.html")

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defun init ()
  "Main program initializations"
  (setq drakma:*text-content-types* (cons '("application" . "json")
					  drakma:*text-content-types*))
  (init-web 4242))

(defun get-dom (url)
  (html5-parser:parse-html5 (drakma:http-request url :cookie-jar *cookie-jar* :user-agent *user-agent*)))

(defun map-dom (recurse-p fn node)
  "fn(node) 
   recurse-p(node) -> map-dom(node)"
  (if node
      (progn
	(funcall fn node)
	(if (funcall recurse-p node)
	    (html5-parser:element-map-children
	     (lambda (n-node) (map-dom recurse-p fn n-node)) node)))))


(defun html-recurse-p (node)
  (not (or (equalp (html5-parser:node-name node) "script")
	   (equalp (html5-parser:node-name node) "style")
	   (equalp (html5-parser:node-name node) "noscript"))))

(defun get-node-name (name node)
  (if (equalp (html5-parser:node-name node) name)
      node))

(defun find-first-node (root recurse-p fn-test)
  (map-dom recurse-p (lambda (n) (if (funcall fn-test n)
					    (return-from find-first-node n))) root))
  
(defun collect-nodes (root fn-test)
  (let (res)
    (labels ((coll (test node)
	       (if node
		   (progn
		     (html5-parser:element-map-children (lambda (n)
							  (coll test n)) node)
		     (if (funcall test node)
			 (push node res))))))
      (coll fn-test root))
    (reverse res)))
 
(defun link-full (url)
    (if (cl-ppcre:all-matches "^https?:\/\/" url)
	(return-from link-full url)
	(return-from link-full (concatenate 'string *uri* url))))

(defun get-post-data (root)
  (let ((nodes (collect-nodes root #'(lambda (n) (get-node-name "input" n)))))
    (cdr (reverse (pairlis (mapcar #'(lambda (n) (html5-parser:element-attribute n "name")) nodes)
	     (mapcar #'(lambda (n) (html5-parser:element-attribute n "value")) nodes))))))

(defun decode-link (uri)
  (let ((res (car (cdr (cl-ppcre:split "r=" uri)))))
    (if res
	(cl-base64:base64-string-to-string  res))))
  
(defun http-post (uri data)
  (drakma:http-request uri :user-agent *user-agent* :method :post :parameters data)) 

(defun make-get-request (url params)
  (let ((res (concatenate 'string url "?")))
    (dolist (args params)
      (setf res (concatenate 'string res (pop args) "=" (substitute #\+ #\Space args) "&")))
    (string-right-trim "&" res)))

(defun make-element-test (elem)
  (if (listp elem)
      `(equalp (html5-parser:element-attribute n ,(pop elem)) ,elem)
      `(html5-parser:element-attribute n ,elem)))

(defun make-type-test (name)
  `(equalp (html5-parser:node-name n) ,name))

(defun node-type-list (names)
  (loop while names
     collecting (make-type-test (pop names))))

(defun node-test-list (fields)
  (loop while fields
     collecting (make-element-test (pop fields))))

(defmacro elements (&rest clauses)
  `#'(lambda (n) (and (equal (html5-parser:node-type n) :element)
		      ,@(node-test-list clauses))))

(defmacro types (&rest types)
  `#'(lambda (n) (or ,@(node-type-list types))))

(defun uri-exists? (uri)
  "Returns 200 if the uri exists, or nil if not found."
  (let ((code (handler-case
		  (nth-value 1 (drakma:http-request uri :cookie-jar *cookie-jar* :user-agent *user-agent* :method :head))
		(usocket:ns-host-not-found-error () 404))))
    (if (equal code 200)
	200
	nil)))

(defun get-header (uri)
  "Returns the header of the uri."
  (handler-case
      (nth-value 2 (drakma:http-request uri :cookie-jar *cookie-jar* :user-agent *user-agent* :method :head))
    (usocket:ns-host-not-found-error () nil)
    (usocket:timeout-error () nil)
    (usocket:host-unreachable-error () nil)))
 
(defun uri-length (uri)
  "Returns the length of data found at uri."
    (parse-integer (cdr (assoc :content-length (get-header uri)))))

(defmacro span (class string)
  `(cl-who:htm (:span :class ,class (cl-who:str ,string))))
