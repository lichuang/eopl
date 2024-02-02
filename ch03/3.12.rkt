#lang racket
#|
Exercise 3.12 [*] Add to the deﬁned language a facility that adds a cond expression. Use the grammar

Expression ::= cond { Expression ==> Expression}∗ end

In this expression, the expressions on the left-hand sides of the ==>’s are evaluated in order 
  until one of them returns a true value. Then the value of the entire expression is the value of 
  the corresponding right-hand expression. If none of the tests succeeds, the expression 
  should report an error.
|#

; see let-lang.rkt