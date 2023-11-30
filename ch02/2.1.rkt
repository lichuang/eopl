#lang racket
;; Exercise 2.1 [*]Implement the four required operations for bigits. 
;; Then use your implementation to calculate the factorial of 10. 
;; How does the execution time vary as this argument changes? 
;; How does the execution time vary as the base changes? Explain why.

(define base 16)
(define bignum-zero '())
(define bignum-zero? null?)

(define bignum-successor (lambda (n) 
  (if (bignum-zero? n)
    '(1)
    (if (eqv? (car n) (- base 1))
      (cons 0 (bignum-successor (cdr n)))
      (cons (+ (car n) 1) (cdr n))
    )
)))

(define bignum-predecessor (lambda (n) 
  (if (bignum-zero? n)
    '()
    (if (eqv? (car n) 0)
      (cons 15 (bignum-predecessor (cdr n)))
      (cons (- (car n) 1) (cdr n))
    )
)))

;; (2 3)
(bignum-successor '(1 3))

;; (0 3)
(bignum-successor '(15 2))

;; (0 0 1)
(bignum-successor '(15 15))

;; (0 3)
(bignum-predecessor '(1 3))

;; (15 2)
(bignum-predecessor '(0 3))