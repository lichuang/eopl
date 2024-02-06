#lang eopl

#|
Exercise 3.20 [*] In PROC, procedures have only one argument, but one can get the effect of 
  multiple argument procedures by using procedures that return other procedures. For example, 
  one might write code like

let f = proc (x) proc (y) ... 
in ((f 3) 4)

This trick is called Currying, and the procedure is said to be Curried. 
Write a Curried procedure that takes two arguments and returns their sum. 
You can write x + y in our language by writing −(x,−(0, y)).
|#

; see extended-proc-lang.rkt

