#lang racket
#|
Exercise 2.7 [*] Rewrite apply-env in ﬁgure 2.1 to give a more informative error message.
|#

(define empty-env '())
(define empty-env? null?)

(define extend-env (lambda (env var val)
  (cons (cons var val) env)
))

(define apply-env (lambda (env var)
  (cond
    [(empty-env? env) (printf "cannot find var ~s" var)]
    [(contain-var? (car env) var) (cdr (car env))]
    [else (apply-env (cdr env) var)])
))

(define contain-var? (lambda (env var)
  (eqv? (car env) var)
))

;; '(("b" . 2) ("a" . 1))
(define my-env (extend-env (extend-env empty-env "a" 1) "b" 2))
my-env

;; 1
(apply-env my-env "a")

;; cannot find var "c"
(apply-env my-env "c")