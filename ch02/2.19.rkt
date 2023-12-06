#lang racket
#|
Exercise 2.19 [*] A binary tree with empty leaves and with interior nodes labeled 
with integers could be represented using the grammar

  Bintree ::= () | (Int Bintree Bintree)

In this representation, implement the procedure number->bintree, which takes a number 
and produces a binary tree consisting of a single node containing that number. 
Also implement current-element, move-to-left-son, move-to-right-son, at-leaf?, insert-to-left, 
and insert-to-right. For example,

> (number->bintree 13)
(13 () ())
> (define t1 (insert-to-right 14
              (insert-to-left 12
                (number->bintree 13))))
> t1
(13
 (12 () ())
 (14 () ()))
> (move-to-left-son t1)
(12 () ())
> (current-element (move-to-left-son t1))
12
> (at-leaf? (move-to-right-son (move-to-left-son t1)))
#t
> (insert-to-left 15 t1)
(13
 (15
  (12 () ())
  ())
 (14 () ()))
|#

(define number->bintree (lambda (value)
  (list value '() '())
))

(define current-element (lambda (tree)
  (root-value tree)
))

(define move-to-left-son (lambda (tree)
  (left-child tree)
))

(define move-to-right-son (lambda (tree)
  (right-child tree)
))

(define at-leaf? (lambda (tree)
  (null? tree)
))

(define root-value (lambda (tree)
  (car tree)
))

(define left-child (lambda (tree)
  (cadr tree)
))

(define right-child (lambda (tree)
  (caddr tree)
))

(define insert-to-right
  (lambda (num tree)
    (let ([root-value (root-value tree)]
          [left-child (left-child tree)]
          [right-child (right-child tree)])
      `(,root-value ,left-child (,num () ,right-child)))))

(define insert-to-left
  (lambda (num tree)
    (let ([root-value (root-value tree)]
          [left-child (left-child tree)]
          [right-child (right-child tree)])
      `(,root-value (,num (), left-child) ,right-child)
)))

(define t1 (insert-to-right 14
            (insert-to-left 12
              (number->bintree 13))))

t1

;; 12
(current-element t1)

;; (12 () ())
(move-to-left-son t1)

#|
(13
 (15
  (12 () ())
  ())
 (14 () ()))
|#
(insert-to-left 15 t1)

;; #t
(at-leaf? (move-to-right-son (move-to-left-son t1)))