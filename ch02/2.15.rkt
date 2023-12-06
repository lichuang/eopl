#lang racket
#|
Exercise 2.15 [*] Implement the lambda-calculus expression interface for the representation 
  speciﬁed by the grammar above.
|#

(define var-exp
  (lambda (var)
    (cons 'var-exp var)
))

(define var-exp?
  (lambda (lc-exp)
    (and (pair? lc-exp) (eqv? (car lc-exp) 'var-exp))
))

(define var-exp->var
  (lambda (lc-exp)
    (cond
      [(var-exp? lc-exp) (cdr lc-exp)]
      [else (printf "~s isnot var-exp" lc-exp)])
))

(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda-exp var lc-exp)
))

(define lambda-exp?
  (lambda (lc-exp)
    (and (pair? lc-exp) (eqv? (car lc-exp) 'lambda-exp))
))

(define lambda-exp->bound-var
  (lambda (lc-exp)
    (cond
      [(lambda-exp? lc-exp) (cadr lc-exp)]
      [else (printf "~s isnot lambda-exp" lc-exp)])
))

(define lambda-exp->body
  (lambda (lc-exp)
    (cond
      [(lambda-exp? lc-exp) (caddr lc-exp)]
      [else (printf "~s isnot lambda-exp" lc-exp)])
))

(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list 'app-exp lc-exp1 lc-exp2)
))

(define app-exp?
  (lambda (lc-exp)
    (and (pair? lc-exp) (eqv? (car lc-exp) 'app-exp))
))

(define app-exp->rator
  (lambda (lc-exp)
    (cond
      [(app-exp? lc-exp) (cadr lc-exp)]
      [else (printf "~s isnot app-exp" lc-exp)]
)))

(define app-exp->rand
  (lambda (lc-exp)
    (cond
      [(app-exp? lc-exp) (caddr lc-exp)]
      [else (printf "~s isnot app-exp" lc-exp)]
)))

(define var-exp-a (var-exp "a"))
;; #t
(var-exp? var-exp-a)
;; "a"
(var-exp->var var-exp-a)

(define lambda-exp-b (lambda-exp "a" var-exp-a))
;; #t
(lambda-exp? lambda-exp-b)
;; "a"
(lambda-exp->bound-var lambda-exp-b)
;; '(var-exp "a")
(lambda-exp->body lambda-exp-b)

(define app-exp-c (app-exp var-exp-a lambda-exp-b))
;; #t
(app-exp? app-exp-c)
;; '(var-exp "a")
(app-exp->rator app-exp-c)
;; '(lambda-exp "a" (var-exp "a"))
(app-exp->rand app-exp-c)