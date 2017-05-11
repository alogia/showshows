(in-package :showshows)
(require :cl-ppcre)

(clsql:def-view-class host-vidtome (host) ())

(defmethod wait-post ((host host-vidtome))
  (with-slots (url) host
    (let ((dom (get-dom url)))
      (html5-parser:parse-html5 
       (progn
	 (sleep 6)
	 (http-post url (subseq (get-post-data dom) 2)))))))

(defmethod parse-video ((host host-vidtome) dom)
  (with-slots (video) host
    (let ((a (find-first-node dom #'html-recurse-p (elements ("class" . "player-url"))))) 
      (if a
	  (setf video (html5-parser:element-attribute a "href"))
	  (print "No link found")))))
