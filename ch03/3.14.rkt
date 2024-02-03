#lang racket
#|
Exercise 3.14 [**] As an alternative to the preceding exercise, add a new nonterminal Bool-exp of 
  boolean expressions to the language. Change the production for conditional expressions to say

Expression ::= if Bool-exp then Expression else Expression

Write suitable productions for Bool-exp and implement value-of-bool-exp. Where do the predicates of 
  exercise 3.8 wind up in this organization?
|#

; see let-lang.rkt `bool-expression` and `value-of-bool-exp` implementation