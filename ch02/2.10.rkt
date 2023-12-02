#lang racket
#|
Exercise 2.10 [*]Add to the environment interface a constructor extend-env*, 
and implement it using the a-list representation. 

This constructor takes a list of variables, a list of values of the same length, 
and an environment, and is specified by
    (extend-env* (var1 … vark) (val1 … valk) ⌈f⌉) = ⌈g⌉,
      where g(var) = vali if var = vari for some i such that 1 ≤ i ≤ k, 
      f(var) otherwise.
|#

(define empty-list? null?)

(define extend-env* (lambda (env vars vals)
  (cond
    [(empty-list? vars) env]
    [else (extend-env* (cons (cons (car vars) (car vals)) env) (cdr vars) (cdr vals))]
  )
))

;; '(("c" . 3) ("b" . 2) ("a" . 1))
;; todo: '(("a" . 1) ("b" . 2) ("c" . 3))?
(extend-env*  '() '("a" "b" "c") '(1 2 3))