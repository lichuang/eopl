#lang racket
#|
Exercise 2.11 [**]A naive implementation of extend-env* from the preceding exercise requires time 
proportional to k to run. It is possible to represent environments so that extend-env* requires 
only constant time: 
represent the empty environment by the empty list, and represent a non-empty environment by the data structure

      ┌───┬───┐
      │ ╷ │ ╶─┼─► saved-env
      └─┼─┴───┘
        ▼
      ┌───┬───┐
      │ ╷ │ ╷ │
      └─┼─┴─┼─┘
    ┌───┘   └───┐
    ▼           ▼
saved-vars  saved-vals

Such an environment might look like

               backbone
                  │
    ┌───┬───┐     ▼     ┌───┬───┐           ┌───┬───┐
    │ ╷ │ ╶─┼──────────►│ ╷ │ ╶─┼──────────►│ ╷ │ ╶─┼──────────► rest of environment
    └─┼─┴───┘           └─┼─┴───┘           └─┼─┴───┘
      ▼                   ▼                   ▼
    ┌───┬───┐           ┌───┬───┐           ┌───┬───┐
    │ ╷ │ ╷ │           │ ╷ │ ╷ │           │ ╷ │ ╷ │
    └─┼─┴─┼─┘           └─┼─┴─┼─┘           └─┼─┴─┼─┘
   ┌──┘   └──┐         ┌──┘   └──┐         ┌──┘   └──┐
   ▼         ▼         ▼         ▼         ▼         ▼
(a b c)  (11 12 13)  (x z)    (66 77)    (x y)    (88 99)
This is called the ribcage representation. The environment is represented as a list of pairs called ribs; 
each left rib is a list of variables and each right rib is the corresponding list of values.

Implement the environment interface, including extend-env*, in this representation.
|#

(define empty-env '())
(define empty-env? null?)

(define apply-env (lambda (env var)
  (cond
    [(empty-env? env) (printf "cannot find var ~s" var)]
    [else 
      (let ([saved-val (ribs-apply-env (car (car env)) (car (cdr (car env))) var)])
        (cond
          [(empty-env? saved-val) (apply-env (cdr env) var)]
          [else saved-val]))
    ]
  )
))

(define ribs-apply-env (lambda (vars vals var)
  (cond
    [(empty-env? vars) empty-env]
    [(contain-var? vars var) (car vals)]
    [else (ribs-apply-env (cdr vars) (cdr vals) var)])
))

(define contain-var? (lambda (vars var)
  (eqv? (car vars) var)
))

(define extend-env (lambda (env vars vals)
  (cons (list vars vals) env)
))

(define extend-env* (lambda (env vars vals)
    (list (list vars vals) env)
))

;; '((("b") (2)) (("a" "c") (1 3)))
(define my-env (extend-env (extend-env empty-env '("a" "c") '(1 3)) '("b") '(2)))
my-env

;; 3
(apply-env my-env "c")

;; '((("a" "b" "c") (1 2 3)) ())
(extend-env*  '() '("a" "b" "c") '(1 2 3))