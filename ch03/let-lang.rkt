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
  (pair-val 
    (first expval?)
    (second expval?))
  (empty-list-val)
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

      [if-exp (condexp exp1 exp2) 
        (let ([val (value-of condexp env)])
          (if (expval->bool val)
            (value-of exp1 env)
            (value-of exp2 env)))]

      [let-exp (vars exps body) 
        (let loop ([vars vars] [exps exps] [old-env env] [new-env env])
          (if (null? vars) (value-of body new-env)
            (let ([var (car vars)] [exp (car exps)])
              (loop 
                (cdr vars) 
                (cdr exps) 
                new-env 
                (extend-env 
                  var 
                  (value-of exp old-env) 
                  new-env)))))]

      [let*-exp (vars exps body) 
        (let loop ([vars vars] [exps exps] [env env])
          (if (null? vars) (value-of body env)
            (let ([var (car vars)] [exp (car exps)])
              (loop 
                (cdr vars) 
                (cdr exps) 
                (extend-env 
                  var 
                  (value-of exp env) 
                  env)))))]
                
      ;; extention of let language
      [minus-exp (exp) 
        (let ([val (value-of exp env)])
          (num-val (- 0 (expval->num val))))]   

      [binary-numerical-exp (op exp1 exp2) 
        (let ([val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)]) 
                (cond
                  [(equal? op "+") (num-val (+ num1 num2))]
                  [(equal? op "*") (num-val (* num1 num2))]
                  [(equal? op "/") (num-val (quotient num1 num2))]
                  [else (eopl:error "unknown binary numerical operator ~s" op)])))]

      [binary-exp (op exp1 exp2) 
        (let ([val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (cond
              [(equal? op "cons") (pair-val val1 val2)]
              [else (eopl:error "unknown binary operator ~s" op)]))]

      [unary-exp (op exp) 
        (let ([val (value-of exp env)])
          (cond
            [(equal? op "car") (
              cases expval val
                (pair-val (first second) first)
                (else (eopl:error "~s is not pair-val" exp)))]

            [(equal? op "cdr") (
              cases expval val
                (pair-val (first second) second)
                (else (eopl:error "~s is not pair-val" exp)))]

            [(equal? op "print") (
              let ([value (
                let loop ([val val])
                  (cases expval val
                    (num-val (number) number)
                    (bool-val (boolean) boolean)
                    (empty-list-val () '())
                    (pair-val (first second) (cons 
                                                (loop first) 
                                                (loop second)))
                    ))])
                (display value)
                (num-val 1))]

            [else (eopl:error "unknown unary operator ~s" op)]                            
          ))]

      [empty-list-exp () (empty-list-val)]

      [n-ary-exp (op exps) 
        (cond
            [(equal? op "list") (
                let loop ([exps exps])
                  (if (null? exps) (empty-list-val)
                    (pair-val (value-of (car exps) env) (loop (cdr exps)))))]
            [else (eopl:error "unknown n-ary operator ~s" op)]                          
          )]

      [bool-exp (exp) (value-of-bool-exp exp env)]

      [cond-exp (lexps rexps) 
        (let loop ([lexps lexps] [rexps rexps])
          (if (null? lexps) (eopl:error "none of conditions eval return true")
            (if (expval->bool (value-of-bool-exp (car lexps) env)) (car rexps)
              (loop (cdr lexps) (cdr rexps)))                
          ))]
    )))

(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-expression exp

      [boolean-exp (exp) 
        (bool-val (expval->bool (value-of exp env)))]      

      [unary-boolean-exp (op exp) 
        (let ([val (value-of exp env)])
          (cond 
            [(equal? op "null?") (
              cases expval val
                (empty-list-val () (bool-val #t))
                (else (bool-val #f)))]
            [(equal? op "zero?") (
              if (zero? (expval->num val)) (bool-val #t)
                  (bool-val #f))]
            [else (eopl:error "unknown unary operator ~s" op)]                            
          ))]

      [binary-boolean-exp (op exp1 exp2) 
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
    [binary-numerical-operator ((or "+" "*" "/")) string]
    [binary-operator ((or "cons")) string]
    [binary-boolean-operator ((or "equal?" "greater?" "less?")) string]
    [unary-operator ((or "car" "cdr" "print")) string]
    [unary-boolean-operator ((or "null?" "zero?")) string]
    [n-ary-operator ("list") string]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]
))

(define the-grammar
  '([program (expression) a-program]
    [expression (number) const-exp]
    (expression (identifier) var-exp)
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]  
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    ;; extention of let language
    [expression ("minus" "(" expression ")") minus-exp]
    [expression (binary-numerical-operator "(" expression "," expression ")") binary-numerical-exp]
    [expression (binary-operator "(" expression "," expression ")") binary-exp]
    [expression (unary-operator "(" expression ")") unary-exp]
    [expression ("emptylist") empty-list-exp]
    [expression (n-ary-operator "(" (separated-list expression ",") ")") n-ary-exp]
    [expression (bool-expression) bool-exp]
    [expression ("cond" (arbno bool-expression "==>" expression) "end") cond-exp]
    [expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp]
    ;; bool-expression
    [bool-expression ("(" expression ")") boolean-exp]
    [bool-expression (unary-boolean-operator "(" expression ")") unary-boolean-exp]
    [bool-expression (binary-boolean-operator "(" expression "," expression ")") binary-boolean-exp]
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
;(display (run "greater? (2, 10)"))
;(display (run "cons (2, 10)"))
;(display (run "cdr (cons (2, 10))"))
;(display (run "null? (emptylist)"))
;(display (run "null? (cons (2, 10))"))
;(display (run "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))"))
;(display (run "let x = 4 in list(x, -(x,1), -(x,3))"))
;(display (run "cond zero?(1) ==> 1 zero?(0) ==> 2 end"))
;(display (run "if 0 then 1 else 2"))
;(display (run "cond (0) ==> 1 (1) ==> 2 end"))
;(display (run "print (let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist)))"))
(display (run "let x = 30 in let x = -(x,1) y = -(x,2) in -(x,y)"))
(display (run "let x = 30 in let* x = -(x,1) y = -(x,2) in -(x,y)"))