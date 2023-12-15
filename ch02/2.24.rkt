#lang eopl

#|
Exercise 2.23 [*] Here is a deﬁnition of binary trees using define-datatype. 
  (define-datatype bintree bintree?
    (leaf-node (num integer?)) 
    (interior-node 
      (key symbol?) 
      (left bintree?) 
      (right bintree?)))

Implement a bintree-to-list procedure for binary trees, so that 
  (bintree-to-list (interior-node ’a (leaf-node 3) (leaf-node 4))) 

returns the list

(interior-node
  a 
  (leaf-node 3) 
  (leaf-node 4))
|#

(define-datatype bintree bintree?
  (leaf-node (num integer?)) 
  (interior-node 
    (key symbol?) 
    (left bintree?) 
    (right bintree?)))

(define bintree-to-list (lambda (tree)
  (list 
    (cases bintree tree
      (leaf-node (num)
        (list 'leaf-node num))
      (interior-node (key left right)
        (list 'interior-node 
          key 
          (bintree-to-list left) 
          (bintree-to-list right)))))))

;; ((interior-node a ((leaf-node 3)) ((leaf-node 4))))
(display (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))))