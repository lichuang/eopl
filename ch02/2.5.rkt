#lang racket
#|
Exercise 2.5 [*] We can use any data structure for representing environments, 
if we can distinguish empty environments from non-empty ones, 
and in which one can extract the pieces of a non-empty environment. 
Implement environments using a representation in which the empty environment is represented as the empty list, 
and in which extend-env builds an environment that looks like

          ┌───┬───┐
          │ ╷ │ ╶─┼─► saved-env
          └─┼─┴───┘
            ▼
          ┌───┬───┐
          │ ╷ │ ╷ │
          └─┼─┴─┼─┘
        ┌───┘   └───┐
        ▼           ▼
    saved-var   saved-val

    This is called an a-list or association-list representation.
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