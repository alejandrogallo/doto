;; [[file:readme.org::*Implementation][Implementation:2]]
;;;; doto.asd
;;
;;;; Copyright (c) 2022 Alejandro Gallo


(asdf:defsystem #:doto
  :description "Describe doto here"
  :author "Alejandro Gallo"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop)
  :components ((:file "package")
               (:file "doto")))
;; Implementation:2 ends here
