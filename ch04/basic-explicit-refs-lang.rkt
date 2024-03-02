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

(define store? (list-of expval?))

; reference? : SchemeVal → Bool 
(define reference? (lambda (v) 
  (integer? v)))

; newref : ExpVal → Ref 
(define (newref val store)
  (let ([next-ref (length store)]
        [new-store (append store (list val))])
    (cons next-ref new-store))) 

; deref : Ref → ExpVal 
(define deref (lambda (ref store) 
  (list-ref store ref)))

; setref! : Ref × ExpVal → Unspeciﬁed 
; usage: sets the-store to a state like the original, but with position ref containing val.
(define (setref! ref val store)
  (let loop ([store1 store]
             [ref1 ref])
    (cond [(null? store1) (report-invalid-reference ref store)]
          [(zero? ref1) (cons val (cdr store1))]
          [else (cons (car store1)
                      (loop (cdr store1) (- ref1 1)))])))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;; ========== interpreter ==========
(define-datatype answer answer?
  [an-answer [val expval?]
             [store store?]])

(define value-of-answer
  (lambda (answer-value)
    (cases answer answer-value
      [an-answer (val store) val])))

(define value-of
  (lambda (exp env store)
    (cases expression exp
      [const-exp (num) (an-answer (num-val num) store)]

      [var-exp (var) (an-answer (apply-env env var) store)]

      [diff-exp (exp1 exp2) 
        (let ([val1 (value-of-answer (value-of exp1 env store))]
              [val2 (value-of-answer (value-of exp2 env store))])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)]) 
                (an-answer (num-val (- num1 num2)) store)))]

      [zero?-exp (exp) 
        (let ([val (value-of-answer (value-of exp env store))])
          (if (zero? (expval->num val)) 
            (an-answer (bool-val #t) store)
            (an-answer (bool-val #f) store)))]

      [if-exp (pred consq alte) 
        (let ([val (value-of-answer (value-of pred env store))])
          (if (expval->bool val)
            (value-of consq env store)
            (value-of alte env store)))]

      [let-exp (var exp body) 
        (cases answer (value-of exp env store)
          [an-answer (val1 store1)
            (value-of body (extend-env var val1 env) store1)])]

      [proc-exp (vars body) 
        (an-answer (proc-val (procedure vars body env)) store)]

      [call-exp (rator rands) 
        (let ([proc (expval->proc (value-of-answer (value-of rator env store)))]
              [args (map (lambda (rand) (value-of-answer (value-of rand env store))) rands)])
            (an-answer (apply-procedure proc args store) store))]

      [letrec-exp (p-names b-vars p-bodies letrec-body) 
        (an-answer (value-of-answer (value-of letrec-body (extend-env-rec p-names b-vars p-bodies env) store)) store)]

      [begin-exp (first-exp left-exps)
        (let loop ([first-exp first-exp] [left-exps left-exps] [store store])
          (let ([current-answer (value-of first-exp env store)])
            (if (null? left-exps)
              current-answer
              (cases answer current-answer
                [an-answer (val store1)
                  (loop (car left-exps) (cdr left-exps) store1)
              ]))))]

      [newref-exp (exp) 
        (cases answer (value-of exp env store)
          [an-answer (val store1)
            (let* ([ref-and-store (newref val store1)]
                   [ref (car ref-and-store)]
                   [store1 (cdr ref-and-store)])
              (an-answer (ref-val ref) store1))])]

      [deref-exp (exp) 
        (cases answer (value-of exp env store)
          [an-answer (val store1) 
            (let ([ref (expval->ref val)])
              (an-answer (deref ref store1) store1))])]

      [setref-exp (exp1 exp2) 
        (cases answer (value-of exp1 env store)
          [an-answer (val1 store1)
            (cases answer (value-of exp2 env store1)
              [an-answer (val2 store2)
                (let* ([ref (expval->ref val1)]
                       [store2 (setref! ref val2 store2)])
                       (an-answer (num-val 23) store2)
                       )])])]
    )))

(define apply-procedure
  (lambda (procVal args store)
    (cases proc procVal
      [procedure (vars body saved-env) 
        (let loop ([env saved-env] [vars vars] [args args])
          (if (null? vars)
            (value-of-answer (value-of body env store))
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
(define value-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (value-of exp1 (init-env) (empty-store))))))

(define run
  (lambda (string)
    (value-of-answer (value-of-program (scan&parse string)))))

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
(display (run "let x = newref(10) in deref(x)"))
(display (run "let x = newref(11) in begin setref(x,1);deref(x) end"))
