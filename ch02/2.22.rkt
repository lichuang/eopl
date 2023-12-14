#lang eopl

#|
Exercise 2.22 [*] Using define-datatype, implement the stack data type of exercise 2.4.
|#

(define value?
  (lambda (v)
    #t))

(define-datatype stack stack?
  (empty-stack-inter)
  (stack-inter
    (_top value?)
    (_stack stack?)))
  
(define empty-stack (lambda () 
  (empty-stack-inter)))

(define push (lambda (stack n) 
  (stack-inter n stack)))

(define pop (lambda (s)
  (cases stack s
    (stack-inter (_top _stack) 
      _stack)
    (empty-stack-inter () 
      (eopl:error "cannot pop an empty stack\n"))
)))

(define top (lambda (s) 
  (cases stack s
    (stack-inter (_top _stack) 
      _top)
    (empty-stack-inter () 
      (eopl:error "cannot top an empty stack\n"))
)))

(define s (empty-stack))

;; cannot top an empty stack
(display (top s))

;; "a"
(display (top (push s "a")))

;; "a"
(display (top (pop (push (push s "a") "b"))))