(in-package :showshows)


(defclass host-vidtome (host) ())

(defmethod wait-post ((host host-vidtome))
  (with-slots (host-url) host
    (http-post host-url
	       (subseq (get-post-data (get-dom host-url)) 2))))
