;; Implementation of basic version of explict-refs language.

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
  (ref-val (ref reference?))
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

(define expval->ref (lambda (expVal)
  (cases expval expVal
    (ref-val (ref) ref)
    (else 
      (eopl:error "~s is not a ref-val" expVal)))))

;; ========== store ==========
(define empty-store (lambda () 
  '()))

; usage: A Scheme variable containing the current state of the store. 
; Initially set to a dummy value.
(define the-store 'uninitialized)

(define get-store (lambda () 
  the-store))

; initialize-store! : () → Unspeciﬁed 
; usage: (initialize-store!) sets the-store to the empty store
(define initialize-store! (lambda () 
  (set! the-store (empty-store))))

; reference? : SchemeVal → Bool 
(define reference? (lambda (v) 
  (integer? v)))

; newref : ExpVal → Ref 
(define newref (lambda (val) 
  (let ([next-ref (length the-store)]) 
    (set! the-store (append the-store (list val))) next-ref)))

; deref : Ref → ExpVal 
(define deref (lambda (ref) 
  (list-ref the-store ref)))

; setref! : Ref × ExpVal → Unspeciﬁed 
; usage: sets the-store to a state like the original, but with position ref containing val.
(define setref!                       
  (lambda (ref val)
    (set! the-store
          (letrec
              ((setref-inner
                ;; returns a list like store1, except that position ref1
                ;; contains val. 
                (lambda (store1 ref1)
                  (cond
                    ((null? store1)
                     (report-invalid-reference ref the-store))
                    ((zero? ref1)
                     (cons val (cdr store1)))
                    (else
                     (cons
                      (car store1)
                      (setref-inner
                       (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;; ========== interpreter ==========
(define value-of-program
  (lambda (pgm)
    (initialize-store!)               ; new for explicit refs.
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

      [begin-exp (first-exp left-exps)
        (let loop ([last-value (value-of first-exp env)] [left-exps left-exps])
          (if (null? left-exps)
            last-value
            (loop (value-of (car left-exps) env) (cdr left-exps))))]

      [newref-exp (exp) 
        (ref-val (newref (value-of exp env)))]


      [deref-exp (exp)
        (let ([value (value-of exp env)])
          (let ([ref (expval->ref value)])
            (deref ref)))]

      [setref-exp (exp1 exp2) 
        (let ([ref (expval->ref (value-of exp1 env))])
          (let ([value (value-of exp2 env)])
            (begin
              (setref! ref value)
              (num-val 23)))
        )]
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
    ; explict ref grammar
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("newref" "(" expression ")") newref-exp]
    [expression ("deref" "(" expression ")") deref-exp]
    [expression ("setref" "(" expression "," expression ")") setref-exp]
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
;explict-refs
(display (run "begin 7; zero?(1) end"))
(display (run "let x = newref(0) in deref(x)"))
(display (run "let x = newref(0) in begin setref(x,1);deref(x) end"))
