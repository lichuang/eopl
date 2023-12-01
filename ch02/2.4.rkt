#lang racket
;; Exercise 2.4 [**] Consider the data type of stacks of values, 
;; with an interface consisting of the procedures empty-stack, push, pop, top, and empty-stack?. 
;; Write a speciﬁcation for these operations in the style of the example above. 
;; Which operations are constructors and which are observers?

(define empty-stack '())

(define empty-stack? null?)

(define push (lambda (stack n) (cons n stack)))

(define pop (lambda (stack) (cdr stack)))

(define top (lambda (stack) (car stack)))

;; (2 1)
(define stack (push (push empty-stack 1) 2))
stack

;; (1)
(pop stack)

;; 2
(top stack)

;; f
(empty-stack? stack)

;; t
(empty-stack? '())