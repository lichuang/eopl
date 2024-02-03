#lang racket
#|
Exercise 3.13 [*] Change the values of the language so that integers are the only expressed values. 
  Modify if so that the value 0 is treated as false and all other values are treated as true. 
  Modify the predicates accordingly.
|#

; see let-lang.rkt 
; i have not changed the values of the language so that integers are the only expressed values,
; instead, i change function `expval->bool` to support number val.
; see test `run "if 0 then 1 else 2"`