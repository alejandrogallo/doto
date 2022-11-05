;; [[file:readme.org::*Packages and asdf][Packages and asdf:4]]
(in-package #:doto)
;; Packages and asdf:4 ends here

;; [[file:readme.org::*Test][Test:1]]
(setq 5am:*run-test-when-defined* t)

(5am:test car-is-fboundp
  (5am:is (car-is-fboundp '(list a b c)))
  (5am:is-false (car-is-fboundp 'list))
  (car-is-fboundp '(fn)))

(5am:test expand
  (macrolet ((is-expand (unexpanded expanded) `(5am:is (equal (expand ,unexpanded)
                                                              ,expanded))))
    (is-expand '(:graph) '(:graph))
    (is-expand '(:graph a b c) '(:graph a b c))
    (is-expand '(:graph (:let ((a :shape)
                               (b :circle))
                          (:cluster my-cluster
                                    (:node :a a b))
                          (:node :shape-lift b)))
               '(:graph ((:cluster my-cluster
                          (:node :a :shape :circle))
                         (:node :shape-lift :circle))))
    (is-expand '(:graph (:scope (:scope (:scope (format nil "Hello world")))))
               '(:graph (:scope (:scope (:scope "Hello world")))))))
;; Test:1 ends here

;; [[file:readme.org::*Test][Test:1]]
(defmacro is-string (list string)
  `(5am:is (string-equal (with-output-to-string (s)
                           (write-dot ,list :stream s))
                         ,string)))

(5am:test symbols
  (is-string :help "help")
  (is-string :help-me-please "helpmeplease")
  (is-string 'help-me-please "HelpMePlease")
  (is-string '|help-me-please| "HelpMePlease")
  (is-string "help-me-please" "\"help-me-please\""))
;; Test:1 ends here

;; [[file:readme.org::*Test][Test:1]]
(5am:test scope
  (is-string '(:scope (:node a) (:node b) (:node c))
             "{A [];B [];C [];}")
  (is-string
   '(:scope (:graph
             ;; :=
             (:= :rank :same)
             (:scope (:node a :color :red)
              ;; cons
              (a b c))
             (:scope (:node this-and-that)
              ;; :list
              (:list e f g))))
   "{graph {rank = same; {A [color=red];A B C} {ThisAndThat [];E,F,G}}}"))
;; Test:1 ends here
