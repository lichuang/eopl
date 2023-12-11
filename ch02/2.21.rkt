#lang eopl

#|
Exercise 2.21 [*] Implement the data type of environments, as in section 2.2.2, using define-datatype.
 Then include has-binding? of exercise 2.9.
|#

(define value?
  (lambda (v)
    #t))

(define-datatype env env?
  (empty-env-inter)
  (apply-env-inter
   (_var symbol?)
   (_env env?))
  (extend-env-inter
   (_var symbol?)
   (_val value?)
   (_env env?))
  (has-binding-inter
   (_var symbol?)
   (_env env?)))

(define empty-env
  (lambda ()
    (empty-env-inter)))

(define extend-env (lambda (var val E)
  (extend-env-inter var val E)))

(define apply-env (lambda (var E)
  (cases env E
	  (empty-env-inter ()
			(eopl:error 'apply-env "Empty env"))
	  (extend-env-inter (_var _val _env)
			(if (eqv? _var var)
				_val
				(apply-env var _env)))
	  (apply-env-inter (_var _env)
			(eopl:error 'apply-env "error"))
	  (has-binding-inter (_var _env)
		  (eopl:error 'apply-env "error")))))

(define has-binding?
  (lambda (var E)
    (cases env E
	   (empty-env-inter () #f)
	   (extend-env-inter (_var _val _env)
			     (if (eqv? _var var)
				 #t
				 (has-binding? var _env)))
	   (apply-env-inter (_var _env)
			    (has-binding? var _env))
	   (has-binding-inter (_var _env)
			      (has-binding? var _env)))))


(define e (empty-env))
(define a (extend-env 'a 1 e))

(has-binding? 'e e)
(has-binding? 'a a)

