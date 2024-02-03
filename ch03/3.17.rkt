#lang racket
#|
Exercise 3.17 [**] Extend the language with a let* expression that works like so that Scheme’s let*
  , so that

let x = 30
in let* x = -(x,1) y = -(x,2) 
  in -(x,y)

should evaluate to 2.
|#

; see let-lang.rkt `let*-exp`