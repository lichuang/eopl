;; Implementation of basic version of trampolined Interpreter.

#lang eopl

;; ========== env ============
(define-datatype environment environment?
  [empty-env]

  [extend-env [var symbol?]
              [val expval?]
              [saved-env environment?]]

  [extend-env-rec
   (p-name symbol?)
   (b-var symbol?)
   (p-body expression?)
   (saved-env environment?)])

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

      [extend-env-rec (p-name b-var p-body saved-env)
        (if (eqv? search-sym p-name)
          (proc-val (procedure b-var p-body env))
          (apply-env saved-env search-sym))])))

;; ========== Implementation of `expval` data type ==========
(define-datatype proc proc?
  (procedure 
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
   (rand expression?)
   (saved-env environment?)
   (saved-cont continuation?)]

  [rand-cont
   (val1 expval?)
   (saved-cont continuation?)]
)

;; ========== interpreter ==========
(define trampoline
  (lambda (bounce)
    (if (expval? bounce)
        bounce
        (trampoline (bounce)))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (trampoline (value-of/k exp1
                                                (init-env)
                                                (end-cont)))])))

;Exp × Env × Cont → Bounce
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

      [call-exp (rator rand)
        (value-of/k rator env
          (rator-cont rand env cont))]

      [letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont)]
)))

;; apply-procedure/k : Proc * ExpVal * Cont -> Bounce
(define apply-procedure/k
  (lambda (procVal arg cont)
    (lambda ()
      (cases proc procVal
        [procedure (var body env) 
          (value-of/k body
                      (extend-env var arg env)
                      cont)]))))

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
      
      [rator-cont (rand saved-env saved-cont)
        (value-of/k rand saved-env
          (rand-cont val saved-cont))]

      [rand-cont (val1 saved-cont)
        (let ((proc (expval->proc val1)))
          (apply-procedure/k proc val saved-cont))]
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
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    ; letrec grammar
    [expression ("letrec" identifier "(" identifier  ")" "=" expression "in" expression) letrec-exp]
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
;(display (run "let f = proc (x,y) -(y,-(x,11)) in (f 77 44)"))
;letrec
(display (run "letrec double (x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 6)"))