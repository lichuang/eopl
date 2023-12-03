#lang racket
;;Exercise 2.12 [*] Implement the stack data type of exercise 2.4 using a procedural representation.

(define empty-stack (lambda ()
  (lambda (cmd)
    (printf "cannot ~s an empty stack\n" cmd)
)))

(define empty-stack? null?)

(define push (lambda (stack saved-val) 
  (lambda (cmd)
    (cond
      [(eqv? cmd "pop") stack]
      [(eqv? cmd "top") saved-val]
    )
  )
))

(define pop (lambda (stack)
  (stack "pop")
))

(define top (lambda (stack)
  (stack "top")
))

(define stack (push (push empty-stack 1) 2))
stack

;; cannot "pop" an empty stack
(pop (empty-stack))

;; 1
(top (pop stack))

;; error(todo)
(pop (pop (pop stack)))