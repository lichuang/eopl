#lang racket
#|
Exercise 3.18 [**] Add an expression to the deﬁned language:

Expression ::= unpack {Identiﬁer}* = Expression in Expression

so that unpack x y z = lst in ... binds x, y, and z to the elements of lst if lst is a list 
  of exactly three elements, and reports an error otherwise. For example, the value of

  let u = 7 
  in unpack x y = cons(u,cons(3,emptylist)) in -(x,y)

should be 4.
|#

; see extended-let-lang.rkt `unpack-exp`