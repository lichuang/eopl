;; Implementation of basic version of imprrative Interpreter.

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
(define exp 'uninitialized) 
(define env 'uninitialized) 
(define cont 'uninitialized) 
(define val 'uninitialized)
(define vals 'uninitialized) 
(define proc1 'uninitialized)

;;value-of-program : Program → FinalAnswer 
(define value-of-program
  (lambda (pgm) 
    (cases program pgm 
      (a-program (exp1) 
        (set! cont (end-cont)) 
        (set! exp exp1) 
        (set! env (init-env)) 
        (value-of/k)))))

(define value-of/k
  (lambda () 
    (cases expression exp
      [const-exp (num) 
        (set! val (num-val num))
        (apply-cont)]

      [var-exp (var) 
        (set! val (apply-env env var))
        (apply-cont)]

      [diff-exp (exp1 exp2) 
        (set! cont (diff1-cont exp2 env cont))
        (set! exp exp1)
        (value-of/k)]

      [zero?-exp (exp1)
        (set! exp exp1)
        (set! cont (zero-cont cont))
        (value-of/k)]

      [if-exp (pred consq alte)
        (set! exp pred)
        (set! cont (if-test-cont consq alte env cont))
        (value-of/k)]

      [let-exp (var exp1 body) 
        (set! exp exp1)
        (set! cont (let-exp-cont var body env cont))
        (value-of/k)]

      [proc-exp (var body) 
        (set! val (proc-val (procedure var body env)))
        (apply-cont)]

      [call-exp (rator rands) 
        (set! cont (rator-cont rands env cont))
        (set! exp rator)
        (value-of/k)]

      [letrec-exp (p-names b-vars p-bodies letrec-body) 
        (set! env (extend-env-rec p-names b-vars p-bodies env))
        (set! exp letrec-body)
        (value-of/k)]
    )))

(define apply-cont
  (lambda () 
    (cases continuation cont
      [end-cont ()
        ;(begin 
        ;(eopl:printf "End of computation.~%") 
        ;val)]
        val]

      [diff1-cont (exp2 saved-env saved-cont)
        (set! exp exp2)
        (set! cont (diff2-cont val saved-cont))
        (set! env saved-env)
        (value-of/k)]

      [diff2-cont (val1 saved-cont)
        (let ([num1 (expval->num val1)] 
              [num2 (expval->num val)])
          (set! cont saved-cont)
          (set! val (num-val (- num1 num2)))
          (apply-cont))]

      [zero-cont (saved-cont)
        (set! cont saved-cont)
        (set! val (bool-val (zero? (expval->num val))))
        (apply-cont)]

      [if-test-cont (exp2 exp3 saved-env saved-cont) 
        (set! cont saved-cont)
        (if (expval->bool val) 
          (set! exp exp2)
          (set! exp exp3))
        (set! env saved-env)
        (value-of/k)]
      
      [let-exp-cont (var body saved-env saved-cont) 
        (set! env (extend-env var val saved-env))
        (set! exp body)
        (set! cont saved-cont)
        (value-of/k)]

      [rator-cont (rands saved-env saved-cont)
                  (if (null? rands)
                      (let ([rator-proc (expval->proc val)])
                        (set! cont saved-cont)
                        (set! proc1 rator-proc)
                        (apply-procedure/k))
                      (begin (set! cont (rand-cont val
                                                   '()
                                                   (cdr rands)
                                                   saved-env
                                                   saved-cont))
                             (set! exp (car rands))
                             (set! env saved-env)
                             (value-of/k)))]

      [rand-cont (rator-val rand-vals rand-exps saved-env saved-cont)
                 (if (null? rand-exps)
                     (let ([rator-proc (expval->proc rator-val)])
                       (set! cont saved-cont)
                       (set! proc1 rator-proc)
                       (set! vals (reverse (cons val rand-vals)))
                       (apply-procedure/k))
                     (begin (set! cont (rand-cont rator-val
                                                  (cons val rand-vals)
                                                  (cdr rand-exps)
                                                  saved-env
                                                  saved-cont))
                            (set! exp (car rand-exps))
                            (set! env saved-env)
                            (value-of/k)))]
    )))

;apply-procedure/k : () → FinalAnswer 
;usage: : relies on registers 
; proc1 : Proc 
; val : ExpVal 
; cont : Cont
(define apply-procedure/k
  (lambda () 
    (cases proc proc1 
      [procedure (vars body saved-env) 
        (set! exp body) 
        (set! env (let loop ( [env saved-env]
                              [vars vars]
                              [vals vals])
                    (if (null? vars)
                      env
                      (loop (extend-env (car vars)
                                        (car vals)
                                        env)
                            (cdr vars)
                            (cdr vals)))))
        (value-of/k)])))

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