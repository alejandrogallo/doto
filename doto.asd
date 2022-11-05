;; [[file:readme.org::*Packages and asdf][Packages and asdf:2]]
;;;; doto.asd
;;
;;;; Copyright (c) 2022 Alejandro Gallo


(asdf:defsystem #:doto
  :description "Doto is a slim library to convert s-expressions into lisp"
  :author "Alejandro Gallo"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop)
  :components ((:file "package")
               (:file "doto")))

(asdf:defsystem #:doto/test
  :description "Tests"
  :author "Alejandro Gallo"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:doto #:fiveam)
  :components ((:file "t")))
;; Packages and asdf:2 ends here
