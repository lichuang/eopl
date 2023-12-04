#lang racket
#|
Exercise 2.14 [**] Extend the representation of the preceding exercise to include a third procedure 
  that implements has-binding?(see exercise 2.9).
|#

(define empty-env
  (lambda ()
    (list
      (lambda (search-var) 
        (printf "No binding for ~s\n" search-var))
      (lambda () #t)
      (lambda (search-var) #f)
)))

(define extend-env (lambda (env var val)
  (list 
    (lambda (search-var)
      (if (eqv? search-var var)
        val
        (apply-env env search-var)))
    (lambda () #f)
    (lambda (search-var)
      (or (eqv? search-var var)
          (has-binding? env search-var)))
)))

(define apply-env (lambda (env var)
  ((car env) var)
))

(define empty-env? (lambda (env)
  ((cadr env))
))

(define has-binding? (lambda (env var)
  ((caddr env) var)
))

(define my-env (extend-env (extend-env (empty-env) "a" 1) "b" 2))
(apply-env my-env "a")

;; #t
(has-binding? my-env "a")

;; #f
(has-binding? my-env "c")