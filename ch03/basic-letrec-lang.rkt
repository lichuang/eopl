;; Implementation of basic version of LETREC language.

#lang eopl

;; ========== env ============
(define-datatype environment environment?
  [empty-env]
  [extend-env [var symbol?]
              [val expval?]
              [saved-env environment?]]
  [extend-env-rec [ids (list-of symbol?)]
                  [bvars (list-of (list-of symbol?))]
                  [bodies (list-of expression?)]
                  [saved-env environment?]])

(define init-env empty-env)

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () 
        (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (var val saved-env)
        (if (eqv? var search-sym)
          val
          (apply-env saved-env search-sym))]
      [extend-env-rec (p-names b-vars p-bodies saved-env) 
        (let loop ([p-names p-names] [b-vars b-vars] [p-bodies p-bodies])
          (if (null? p-names)
            (apply-env saved-env search-sym)
            (if (eqv? search-sym (car p-names))
              (proc-val (procedure (car b-vars) (car p-bodies) env))
              (loop (cdr p-names) (cdr b-vars) (cdr p-bodies)))))])))

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

      [if-exp (pred consq alte) 
        (let ([val (value-of pred env)])
          (if (expval->bool val)
            (value-of consq env)
            (value-of alte env)))]

      [let-exp (var exp body) 
        (let ([val (value-of exp env)])
          (let ([arg (extend-env var val env)])
            (value-of body arg)))]

      [proc-exp (vars body) 
        (proc-val (procedure vars body env))]

      [call-exp (rator rands) 
        (let ([proc (expval->proc (value-of rator env))]
              [args (map (lambda (rand) (value-of rand env)) rands)])
            (apply-procedure proc args))]

      [letrec-exp (p-names b-vars p-bodies letrec-body) 
        (value-of letrec-body (extend-env-rec p-names b-vars p-bodies env))]
    )))

(define apply-procedure
  (lambda (procVal args)
    (cases proc procVal
      [procedure (vars body saved-env) 
        (let loop ([env saved-env] [vars vars] [args args])
          (if (null? vars)
            (value-of body env)
            (loop (extend-env (car vars) (car args) env)
                  (cdr vars)
                  (cdr args))))])))

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
    ; proc grammar
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    ; letrec grammar
    [expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp]
))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;========== run ============
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;========== test ============
; let lang
(display (run "7"))
(display (run "zero? (1)"))
(display (run "let a = 1 in zero? (a)"))
(display (run "let a = 1 in -(a,2)"))
(display (run "if zero? (1) then 1 else 2"))
; proc lang
(display (run "let f = proc (x) -(x,11) in (f 77)"))
(display (run "let f = proc (x,y) -(y,-(x,11)) in (f 77 44)"))
;letrec
(display (run "letrec double (x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 6)"))