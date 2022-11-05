;; [[file:readme.org::*Packages and asdf][Packages and asdf:1]]
;;;; package.lisp
;;
;;;; Copyright (c) 2022 Alejandro Gallo


(defpackage #:doto
  (:use #:cl)
  (:export
   #:write-dot
   #:with-dot
   #:with-dot-to-string
   #:with-dot-to-svg
   #:with-dot-to-x11))
;; Packages and asdf:1 ends here
