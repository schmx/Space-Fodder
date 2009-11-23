;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :space-fodder
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "space-fodder"))
  :depends-on (:until-it-dies))
