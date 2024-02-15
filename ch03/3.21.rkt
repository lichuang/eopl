#|
Exercise 3.21 [**] Extend the language of this section to include procedures with multiple arguments 
  and calls with multiple operands, as suggested by the grammar

  Expression ::= proc ( { Identiﬁer } ∗ ( , ) ) Expression 
  Expression ::= (Expression { Expression } ∗ )
|#

#lang eopl

;; ========== env ============
(define empty-env-record
  (lambda ()
    '()))

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env?
  (lambda (x)
    (empty-env-record? x)))

(define empty-env-record? null?)

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ([sym (extended-env-record->sym env)]
              [val (extended-env-record->val env)]
              [old-env (extended-env-record->old-env env)])
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

(define contain-var? (lambda (env var)
  (eqv? (car env) var)
))

(define init-env empty-env)

;; ========== Implementation of `expval` data type ==========
(define-datatype proc proc?
  (procedure 
    (vars (list-of symbol?))
    (body expression?) 
    (env environment?))
)

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (boolean boolean?))
  (proc-val (proc proc?))
)

(define expval->num (lambda (expVal)
  (cases expval expVal 
    (num-val (num)
      num)
    (else 
      (eopl:error "~s is not a num-val" expVal)
))))

(define expval->bool (lambda (expVal)
  (cases expval expVal 
    (bool-val (boolean)
      boolean)
    (num-val (num)
      (if (zero? num) #f
        #t))
    (else 
      (eopl:error "~s is not bool-val" expVal)
))))

(define expval->proc (lambda (expVal)
  (cases expval expVal 
    (proc-val (proc)
      proc)
    (else 
      (eopl:error "~s is not a proc-val" expVal)
))))

;; ========== interpreter ==========
(define value-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]

      [var-exp (id) (apply-env env id)]

      [diff-exp (exp1 exp2) 
        (let ([val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)]) 
                (num-val (- num1 num2))))]

      [zero?-exp (exp) 
        (let ([val (value-of exp env)])
          (if (zero? (expval->num val)) (bool-val #t)
            (bool-val #f)))]

      [if-exp (condexp exp1 exp2) 
        (let ([val (value-of condexp env)])
          (if (expval->bool val)
            (value-of exp1 env)
            (value-of exp2 env)))]

      [let-exp (var exp body) 
        (let ([val (value-of exp env)])
          (let ([arg (extend-env var val env)])
            (value-of body arg)))]

      [proc-exp (vars body) 
        (proc-val (procedure vars body env))]

      [call-exp (rator rands) 
        (let ([proc (expval->proc (value-of rator env))])
            (apply-procedure proc rands env))]
    )))

(define apply-procedure
  (lambda (procVal rands env)
    (cases proc procVal
      (procedure (proc-vars proc-body proc-env)
        (let loop ([proc-vars proc-vars] [rands rands] [env proc-env])
          (if (null? rands) (value-of proc-body env)
            (loop (cdr proc-vars) (cdr rands) (extend-env (car proc-vars) (value-of (car rands) env) env)))))
      (else
        (eopl:error "~s is not proc-val" procVal))
)))

;; ========== lexical specification and grammar ==========
(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]
))

(define the-grammar
  '([program (expression) a-program]
    ; basic let grammar
    [expression (number) const-exp]
    (expression (identifier) var-exp)
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]  
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;========== run ============
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;========== test ============
(display (run "let f = proc (x,y) -(y,-(x,11)) in (f 77 44)"))


