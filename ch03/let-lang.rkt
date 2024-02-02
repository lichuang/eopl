#lang eopl

;; Implementation of extended version of LET language.

;; ========== env ============
(define empty-env-record
  (lambda ()
    '()))

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
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (boolean boolean?))
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
    (else 
      (eopl:error "~s is not bool-val" expVal)
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

      [zero?-exp (exp)
        (let ([val (value-of exp env)]) 
          (let ([num (expval->num val)]) 
            (if (zero? num)
              (bool-val #t)
              (bool-val #f))))]

      [diff-exp (exp1 exp2) 
        (let ([val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)]) 
                (num-val (- num1 num2))))]

      [if-exp (condexp exp1 exp2) 
        (let ([val (value-of condexp env)])
          (if (expval->bool val)
            (value-of exp1 env)
            (value-of exp2 env)))]

      [let-exp (id exp body) 
        (let ([val (value-of exp env)])
          (value-of body (extend-env id val env)))]

      ;; extention of let language
      [minus-exp (exp) 
        (let ([val (value-of exp env)])
          (num-val (- 0 (expval->num val))))]   

      [binary-exp (op exp1 exp2) 
        (let ([val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)]) 
                (cond
                  [(equal? op "+") (num-val (+ num1 num2))]
                  [(equal? op "*") (num-val (* num1 num2))]
                  [(equal? op "/") (num-val (quotient num1 num2))]
                  [else (eopl:error "unknown binary operator ~s" op)])))]

      [compare-exp (op exp1 exp2) 
        (let ([val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)]) 
                (cond
                  [(equal? op "equal?") (bool-val (equal? num1 num2))]
                  [(equal? op "greater?") (bool-val (> num1 num2))]
                  [(equal? op "less?") (bool-val (< num1 num2))]
                  [else (eopl:error "unknown compare operator ~s" op)])))]
    )))

;; ========== lexical specification and grammar ==========
(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [binary-operator ((or "+" "*" "/")) string]
    [compare-operator ((or "equal?" "greater?" "less?")) string]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]
))

(define the-grammar
  '([program (expression) a-program]
    [expression (number) const-exp]
    (expression (identifier) var-exp)
    [expression ("zero?" "(" expression ")") zero?-exp]  
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]  
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    ;; extention of let language
    [expression ("minus" "(" expression ")") minus-exp]
    [expression (binary-operator "(" expression "," expression ")") binary-exp]
    [expression (compare-operator "(" expression "," expression ")") compare-exp]
))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;========== run ============
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;========== test ============
;(display (run "0"))
;(display (run "zero? (1)"))
;(display (run "- (1, 10)"))
;(display (run "if zero? (1) then 1 else 2"))
;(display (run "let x = 10 in - (1, x)"))
;(display (run "minus(-(minus(5),9))"))
;(display (run "/ (2, 10)"))
(display (run "greater? (2, 10)"))
