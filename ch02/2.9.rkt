#lang racket
#|
Exercise 2.9 [*] Add to the environment interface an observer called has-binding? 
that takes an environment env and a variable s and tests to see if s has an associated value in env. 
Implement it using the a-list representation.
|#

(define empty-env? null?)

(define has-binding? (lambda (env var)
  (cond
    [(empty-env? env) #f]
    [(eqv? (car (car env)) var) #t]
    [else (has-binding? (cdr env) var)]
)))

;; #t
(has-binding? (list (list "a" 1) (list "b" 2)) "b")

;; #f
(has-binding? (list (list "a" 1) (list "b" 2)) "c")