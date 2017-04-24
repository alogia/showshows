(in-package :showshows)

;;; Abstract classes for printable objects.
;;; Assumes all objects are printable to text and to html.

(defclass echos ()
  ())

(defgeneric echo (echos)
    (:documentation "Abstract function for printable objects to text"))

(defgeneric echo-html (echos)
  (:documentation "Abstract function for printable objects to html"))
