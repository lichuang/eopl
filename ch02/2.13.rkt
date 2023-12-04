#lang racket
#|
Exercise 2.13 [**] Extend the procedural representation to implement empty-env? 
  by representing the environment by a list of two procedures: 
    one that returns the value associated with a variable, as before, 
    and one that returns whether or not the environment is empty.
|#

(define empty-env
  (lambda ()
    (list
      (lambda (search-var) 
        (printf "No binding for ~s\n" search-var))
      (lambda () #t)
)))

(define extend-env (lambda (env var val)
  (list 
    (lambda (search-var)
      (if (eqv? search-var var)
        val
        (apply-env env search-var)))
    (lambda () #f)
)))

(define apply-env (lambda (env var)
  ((car env) var)
))

(define empty-env? (lambda (env)
  ((cadr env))
))

(define my-env (extend-env (extend-env (empty-env) "a" 1) "b" 2))

;; #f
(empty-env? my-env)

;; 1
(apply-env my-env "a")

;; No binding for "c"
(apply-env my-env "c")