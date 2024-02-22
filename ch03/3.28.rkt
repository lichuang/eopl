#|
Exercise 3.28 [**] Dynamic binding (or dynamic scoping) is an alternative design for procedures, 
  in which the procedure body is evaluated in an environment obtained by extending the environment 
  at the point of call. For example in

let a = 3
in let p = proc (x) -(x,a)
    a = 5
  in -(a,(p 2))

the a in the procedure body would be bound to 5, not 3. Modify the language to use dynamic binding. 
Do this twice, once using a procedural representation for procedures, 
and once using a data-structure representation.
|#

#lang eopl

;; Implementation of basic version of PROC language.

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
    ;(vars (list-of symbol?))
    (var symbol?)
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

      [if-exp (pred consq alte) 
        (let ([val (value-of pred env)])
          (if (expval->bool val)
            (value-of consq env)
            (value-of alte env)))]

      [let-exp (vars exps body) 
        (let loop ([vars vars] [exps exps] [body body] [env env])
          (if (null? vars) (value-of body env)
            (loop 
              (cdr vars) 
              (cdr exps) 
              body 
              (let ([var (car vars)] [val (value-of (car exps) env)])
                  (extend-env var val env)))))]

      [proc-exp (var body) 
        (proc-val (procedure var body env))]

      [call-exp (rator rand) 
        (let ([proc (expval->proc (value-of rator env))])
          (apply-procedure proc (value-of rand env)))]
    )))

(define apply-procedure
  (lambda (procVal value)
    (cases proc procVal
      (procedure (var body env)
        (value-of body (extend-env var value env)))
      (else 
        (eopl:error "~s is not proc-val" procVal)
    ))))

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
    ; let grammar
    [expression (number) const-exp]
    (expression (identifier) var-exp)
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]  
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    ; proc grammar
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;========== run ============
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;========== test ============
(display (run "let a = 3 in let p = proc (x) -(x,a) a = 5 in -(a,(p 2))"))
(display (run "let a = 1 in zero? (a)"))
;(display (run "let a = 3 in let p = proc (x) -(x,a) in -(a,(p 2))"))