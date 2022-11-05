;; [[file:readme.org::*Packages and asdf][Packages and asdf:3]]
;;;; doto.lisp
;;
;;;; Copyright (c) 2022 Alejandro Gallo
(in-package #:doto)
;; Packages and asdf:3 ends here

;; [[file:readme.org::*The expander][The expander:1]]
(defvar *PD* (copy-pprint-dispatch))
;; The expander:1 ends here

;; [[file:readme.org::*The expander][The expander:2]]
(defun car-is-fboundp (x)
  (and (consp x)
       (symbolp (car x))
       (fboundp (car x))))

(defun expand (list)
  (etypecase list
    ((or (satisfies car-is-fboundp)
         (cons (member :listp)))
     (expand (eval list)))
    ((cons (member :let))
     (destructuring-bind (_ bindings &rest body) list
       (declare (ignore _))
       (expand (sublis (mapcar (lambda (bs) (cons (car bs)
                                                  (cadr bs)))
                               bindings)
                       body
                       :test #'equal))))
    (cons (mapcar #'expand list))
    (t list)))
;; The expander:2 ends here

;; [[file:readme.org::*Writer API][Writer API:1]]
(defun write-dot (sexpr &rest args)
  (apply #'write (expand sexpr) :pretty t
                                :pprint-dispatch *PD*
                                args))

(defmacro with-dot (sexpr)
  `(write-dot ',sexpr))

(defmacro with-dot-to-string (sexpr)
  (let ((stream (gensym)))
    `(with-output-to-string (,stream)
       (write-dot ',sexpr :stream ,stream))))

(defmacro with-dot-to-x11 (sexpr)
  (let ((out (gensym "output"))
        (in (gensym "input")))
    `(with-input-from-string (,in (with-output-to-string (,out)
                                    (write-dot ',sexpr :stream ,out)))
       (uiop:run-program '("dot" "-Tx11")
                         :input ,in))))

(defmacro with-dot-to-svg (sexpr)
  (let ((dot-out (gensym "output"))
        (svg-out (gensym "svg"))
        (in (gensym "input")))
    `(with-input-from-string (,in (with-output-to-string (,dot-out)
                                    (write-dot ',sexpr :stream ,dot-out)))
       (with-output-to-string (,svg-out)
           (uiop:run-program '("dot" "-Tsvg")
                             :input ,in
                             :output ,svg-out)))))
;; Writer API:1 ends here

;; [[file:readme.org::*Atoms][Atoms:1]]
(set-pprint-dispatch 'keyword
                     (lambda (s sbl)
                       (write-string
                        (remove #\- (string-downcase sbl))
                        s))
                     1 *PD*)

(set-pprint-dispatch 'symbol
                     (lambda (s sbl)
                       (write-string
                        (remove #\- (string-capitalize sbl))
                        s))
                     0 *PD*)
;; Atoms:1 ends here

;; [[file:readme.org::*Scope][Scope:1]]
(defun pr-scope (s list)
  (format s "~<~*~1@{{~2i~_~
             ~@{~W~^~_~}~
             ~I~_}~}~:>"
          list))

(set-pprint-dispatch '(cons (member :scope))
                     #'pr-scope
                     0 *PD*)

(set-pprint-dispatch '(cons (member :graph))
                     (lambda (s list)
                       (let* ((type (string-downcase (string (car list))))
                              (name (when (atom #1=(cadr list))
                                      #1#))
                              (body (if name
                                        (cddr list)
                                        (cdr list))))
                         (format s "~a " type)
                         (when name
                           (format s "~W " name))
                         (pr-scope s (list :scope body))))
                     0 *PD*)

(set-pprint-dispatch '(cons (member :list))
                     (formatter "~<~*~@{~w~^,~}~:>")
                     0 *PD*)


(set-pprint-dispatch '(cons (member :=))
                     (formatter "~{~*~W = ~W;~}")
                     0 *PD*)

(set-pprint-dispatch 'cons
                     (formatter "~<~@{~w~^ ~_~}~:>")
                     -1 *PD*)
;; Scope:1 ends here

;; [[file:readme.org::*Options][Options:1]]
(defun pr-options (s list &rest ignore)
  (declare (ignore ignore))
  (format s "~<[~
             ~1i~@{~W=~W~^, ~_~}~
             ]~:>"
          list))

(set-pprint-dispatch '(cons (member :node-opts :edge-opts :graph-opts))
                     (lambda (s list)
                       (format s "~(~a~) "
                               (subseq (string (car list))
                                       0
                                       (position #\- (string (car list)))))
                       (pr-options s (cdr list))
                       (write-string ";" s))
                     0 *PD*)
;; Options:1 ends here

;; [[file:readme.org::*Nodes and edges][Nodes and edges:1]]
(set-pprint-dispatch '(cons (member :node))
                     (lambda (s list)
                       (format s "~w " (cadr list))
                       (pr-options s (cddr list))
                       (write-string ";" s))
                     0 *PD*)

(set-pprint-dispatch '(cons (member :cluster))
                     (lambda (s list)
                       (format s "subgraph cluster_~W ~W"
                               (cadr list)
                               (list :scope (cddr list))))
                     0 *PD*)

(set-pprint-dispatch '(cons (member :-- :-> :<-))
                     (lambda (s list)
                       (destructuring-bind (type from to &rest opts) list
                         (case type
                           (:-> (setq opts (append '(:dir "forward") opts)))
                           (:<- (setq opts (append '(:dir "back") opts))))
                         (format s "~<~
                                    ~w~_ --~_ ~w ~
                                    ~/doto::pr-options/;~
                                    ~:>"
                                 (list (list :scope from) (list :scope to) opts))))
                     0 *PD*)
;; Nodes and edges:1 ends here
