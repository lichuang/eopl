#lang racket
;; Exercise 2.4 [**] Consider the data type of stacks of values, 
;; with an interface consisting of the procedures empty-stack, push, pop, top, and empty-stack?. 
;; Write a speciﬁcation for these operations in the style of the example above. 
;; Which operations are constructors and which are observers?

(define empty-stack '())

(define empty-stack? null?)

(define push (lambda (stack n) (cons n stack)))

(define pop (lambda (stack)
  (cond
    [(empty-stack? stack) (printf "cannot pop an empty stack\n")]
    [else (cdr stack)]
  )
))

(define top (lambda (stack) 
  (cond
    [(empty-stack? stack) (printf "cannot top an empty stack\n")]
    [else (car stack)]
  )
))

;; (2 1)
(define stack (push (push empty-stack 1) 2))
stack

;; (1)
(pop stack)

;; "cannot pop an empty stack"
(pop empty-stack)

;; "cannot top an empty stack"
(top empty-stack)

;; 2
(top stack)

;; f
(empty-stack? stack)

;; t
(empty-stack? '())