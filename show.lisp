(in-package :showshows)

(defun create-episode (show season num name date url hosts)
  "Acons list to represent an episode"
  (list :show show :season season :num num :name name :date date :url url :hosts hosts))

(defun create-season (show num episodes)
  "Acons list to represent a season"
  (list (cons :show show) (cons :num num) (cons :episodes episodes)))

(defun create-show (name url seasons)
  "Acons list to represent a show"
  (list (cons :name name) (cons :url url) (cons :seasons seasons)))
  
