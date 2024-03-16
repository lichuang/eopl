;; Implementation of basic version of Continuation-Passing Interpreter.

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

;; ========== `continuation` data type ==========
(define-datatype continuation continuation?
  [end-cont]

  [diff1-cont
    (exp2 expression?) 
    (env environment?) 
    (cont continuation?)]

  [diff2-cont
    (val1 expval?) 
    (cont continuation?)]

  [zero-cont 
    (cont continuation?)]

  [if-test-cont 
    (exp2 expression?) 
    (exp3 expression?) 
    (env environment?) 
    (cont continuation?)]

  [let-exp-cont 
    (var symbol?) 
    (body expression?) 
    (env environment?) 
    (cont continuation?)]
  
  [rator-cont
    (rands (list-of expression?))
    (env environment?) 
    (cont continuation?)]

  [rand-cont
    (proc expval?)
    (vals (list-of expval?))
    (rands (list-of expression?))
    (env environment?) 
    (cont continuation?)]
)

;; ========== interpreter ==========
(define value-of-program
  (lambda (pgm) 
    (cases program pgm 
      (a-program (exp1) 
        (value-of/k exp1 (init-env) (end-cont))))))

(define value-of/k
  (lambda (exp env cont) 
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]

      [var-exp (var) (apply-cont cont (apply-env env var))]

      [diff-exp (exp1 exp2) 
        (value-of/k exp1 env (diff1-cont exp2 env cont))]

      [zero?-exp (exp1)
        (value-of/k exp1 env (zero-cont cont))]

      [if-exp (pred consq alte) 
        (value-of/k pred env (if-test-cont consq alte env cont))]

      [let-exp (var exp1 body) 
        (value-of/k exp1 env (let-exp-cont var body env cont))]

      [proc-exp (var body) 
        (apply-cont cont (proc-val (procedure var body env)))]

      [call-exp (rator rands) 
        (value-of/k rator env (rator-cont rands env cont))]

      [letrec-exp (p-names b-vars p-bodies letrec-body) 
        (value-of/k letrec-body (extend-env-rec p-names b-vars p-bodies env) cont)]
)))

(define apply-procedure/k
  (lambda (procVal args cont)
    (cases proc procVal
      [procedure (vars body saved-env) 
        (let loop ([env saved-env] [vars vars] [args args])
          (if (null? vars)
            (value-of/k body env cont)
            (loop (extend-env (car vars) (car args) env)
                  (cdr vars)
                  (cdr args))))])))

(define apply-cont
  (lambda (cont val) 
    (cases continuation cont
      [end-cont ()
        ;(begin 
        ;(eopl:printf "End of computation.~%") 
        ;val)]
        val]

      [diff1-cont (exp2 env cont)
        (value-of/k exp2 env (diff2-cont val cont))]

      [diff2-cont (val1 cont)
        (let ([num1 (expval->num val1)] [num2 (expval->num val)])
          (apply-cont cont (num-val (- num1 num2))))]

      [zero-cont (saved-cont)
        (apply-cont saved-cont (bool-val (zero? (expval->num val))))]
      
      [if-test-cont (exp2 exp3 saved-env saved-cont) 
        (if (expval->bool val) 
          (value-of/k exp2 saved-env saved-cont) 
          (value-of/k exp3 saved-env saved-cont))]
      
      [let-exp-cont (var body saved-env saved-cont) 
        (value-of/k body (extend-env var val saved-env) saved-cont)]
      
      [rator-cont (rands saved-env saved-cont)
        (if (null? rands)
          (apply-procedure/k val '() saved-cont)
          (value-of/k (car rands) 
                      saved-env
                      (rand-cont val 
                                 '() 
                                 (cdr rands)
                                 saved-env
                                 saved-cont)))]

      [rand-cont (proc vals rands saved-env saved-cont)
        (if (null? rands)
          (apply-procedure/k 
            (expval->proc proc) 
            (reverse (cons val vals)) 
            saved-cont)
          (value-of/k (car rands) 
                      saved-env
                      (rand-cont proc 
                                 (cons val vals) 
                                 (cdr rands)
                                 saved-env
                                 saved-cont)))]
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
(display (run "-(7,1)"))
(display (run "zero? (1)"))
(display (run "let a = 1 in zero? (a)"))
(display (run "let a = 1 in -(a,2)"))
(display (run "if zero? (1) then 1 else 2"))
; proc lang
(display (run "let f = proc (x) -(x,11) in (f 77)"))
(display (run "let f = proc (x,y) -(y,-(x,11)) in (f 77 44)"))
;letrec
(display (run "letrec double (x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 6)"))