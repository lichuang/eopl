#lang racket
#|
Exercise 3.6 [*] Extend the language by adding a new operator minus that takes one argument, n, 
  and returns −n. For example, the value of minus(-(minus(5),9)) should be 14.
|#

; see let-lang.rkt `minus-exp` implementation