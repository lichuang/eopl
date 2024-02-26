#lang eopl

;; Implementation of basic version of lexical addressing language.

;; ========== static env ============
(define-datatype senv senv?
  [empty-senv]
  [extend-senv  
    [var symbol?]
    [saved-senv senv?]])

(define init-senv empty-senv)

(define apply-senv
  (lambda (static-env sym)
    (cases senv static-env
      [empty-senv () 
        (eopl:error 'apply-senv "No binding for ~s" sym)]
      [extend-senv (var saved-senv)
        (if (eqv? var sym)
          0
          (+ 1 (apply-senv saved-senv sym)))])))

;; ========== nameless environment ============
(define-datatype nameless-environment nameless-environment?
  [empty-nameless-env]
  [extend-nameless-env
    [val expval?]
    [saved-env nameless-environment?]])

(define init-nameless-env empty-nameless-env)

(define apply-nameless-env
  (lambda (env lex-addr)
    (cases nameless-environment env
      [empty-nameless-env () 
        (eopl:error 'apply-nameless-env "No binding for ~s" lex-addr)]
      [extend-nameless-env (val saved-env)
        (if (eqv? lex-addr 0)
          val
          (apply-nameless-env saved-env (- lex-addr 1)))])))

(define apply-procedure
  (lambda (procVal value)
    (cases proc procVal
      (procedure (body saved-nameless-env)
        (value-of body (extend-nameless-env value saved-nameless-env)))
      (else 
        (eopl:error "~s is not proc-val" procVal)
    ))))
;; ========== Implementation of `expval` data type ==========
; procedure : Nameless-exp × Nameless-env → Proc
(define-datatype proc proc?
  (procedure 
    (body expression?) 
    (env nameless-environment?))
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
		      (value-of exp1 (init-nameless-env))))))

; translation-of : Exp × Senv → Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      [const-exp (num) (const-exp num)]

      [var-exp (var) 
        (nameless-var-exp (apply-senv senv var))]

      [diff-exp (exp1 exp2) 
        (diff-exp 
          (translation-of exp1 senv)
          (translation-of exp2 senv))]

      [zero?-exp (exp) 
        (zero?-exp (translation-of exp senv))]

      [if-exp (condexp exp1 exp2) 
        (if-exp
          (translation-of condexp senv)
          (translation-of exp1 senv)
          (translation-of exp2 senv))]

      [let-exp (var exp body) 
        (nameless-let-exp
          (translation-of exp senv)
          (translation-of body (extend-senv var senv)))]

      [proc-exp (var body) 
        (nameless-proc-exp
          (translation-of body (extend-senv var senv)))]

      [call-exp (rator rand) 
        (call-exp
          (translation-of rator senv)
          (translation-of rand senv))]

      ; other expression report source error
      [else
        (eopl:error "invalid-source-expression ~s" exp)]
    )))

; value-of : Nameless-exp x Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      [const-exp (num) (num-val num)]

      [diff-exp (exp1 exp2) 
        (let ([val1 (value-of exp1 nameless-env)]
              [val2 (value-of exp2 nameless-env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)]) 
                (num-val (- num1 num2))))]

      [zero?-exp (exp) 
        (let ([val (value-of exp nameless-env)])
          (if (zero? (expval->num val)) (bool-val #t)
            (bool-val #f)))]

      [if-exp (condexp exp1 exp2) 
        (let ([val (value-of condexp nameless-env)])
          (if (expval->bool val)
            (value-of exp1 nameless-env)
            (value-of exp2 nameless-env)))]

      [call-exp (rator rand) 
        (let ([proc (expval->proc (value-of rator nameless-env))])
          (apply-procedure proc (value-of rand nameless-env)))]

      ; nameless expression implementation
      [nameless-var-exp (num)
        (apply-nameless-env nameless-env num)]

      [nameless-let-exp (exp body) 
        (let ([val (value-of exp nameless-env)])
          (let ([arg (extend-nameless-env val nameless-env)])
            (value-of body arg)))]

      [nameless-proc-exp (body) 
        (proc-val
          (procedure body nameless-env))]
      
      ; other expression report translate error
      [else
        (eopl:error "invalid-translated-expression ~s" exp)]
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
    ; let grammar
    [expression (number) const-exp]
    (expression (identifier) var-exp)
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]  
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    ; proc grammar
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    ; nameless gramar
    [expression ("%letref" number) nameless-var-exp]
    [expression ("%let" expression "in" expression) nameless-let-exp]
    [expression ("%lexproc" expression) nameless-proc-exp]
))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;========== run ============
; Program → Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (a-program (translation-of exp1 (init-senv)))])))

(define value-of-translation
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (init-nameless-env))])))

(define run
  (lambda (string)
    (value-of-translation
     (translation-of-program
      (scan&parse string)))))

;========== test ============
(display (run "7"))
(display (run "- (7, 1)"))
(display (run "zero? (1)"))
(display (run "let a = 1 in zero? (a)"))
(display (run "let a = 1 in -(a,2)"))
(display (run "if zero? (1) then 1 else 2"))
(display (run "let f = proc (x) -(x,11) in (f 77)"))