;; [[file:readme.org::*Implementation][Implementation:3]]
;;;; doto.lisp
;;
;;;; Copyright (c) 2022 Alejandro Gallo
(in-package #:doto)
;; Implementation:3 ends here

;; [[file:readme.org::*Implementation][Implementation:4]]
(defvar *PD* (copy-pprint-dispatch))
;; Implementation:4 ends here

;; [[file:readme.org::*Implementation][Implementation:5]]
(defun car-is-fboundp (x)
  (and (consp x)
       (symbolp (car x))
       (fboundp (car x))))

(defun expand (list)
  (etypecase list
    ((satisfies car-is-fboundp) (expand (eval list)))
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
;; Implementation:5 ends here

;; [[file:readme.org::*Implementation][Implementation:6]]
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
;; Implementation:6 ends here
