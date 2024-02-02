#lang racket
#|
Exercise 3.10 [**] Add an operation list to the language. This operation should take any number 
  of arguments, and return an expressed value containing the list of their values. For example,

let x = 4
in list(x, -(x,1), -(x,3))

should return an expressed value that represents the list (4 3 1).
|#

; see let-lang.rkt `n-ary-exp`