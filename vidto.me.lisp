(in-package :showshows)

(defun decode-link (uri)
  (let ((part (car (cdr (cl-ppcre:split "r=" uri)))))
    (cl-base64:base64-string-to-string part)))
