#lang eopl

#|
Exercise 2.25 [**] Use cases to write max-interior, which takes a binary tree of integers 
  (as in the preceding exercise) with at least one interior node and returns the symbol 
  associated with an interior node with a maximal leaf sum.

    > (define tree-1
        (interior-node 'foo (leaf-node 2) (leaf-node 3)))
    > (define tree-2
        (interior-node 'bar (leaf-node -1) tree-1))
    > (define tree-3
        (interior-node 'baz tree-2 (leaf-node 1)))
    > (max-interior tree-2)
    foo
    > (max-interior tree-3)
    baz

    The last invocation of max-interior might also have returned foo, 
    since both the foo and baz nodes have a leaf sum of 5.
|#

(define-datatype bintree bintree?
  (leaf-node (num integer?)) 
  (interior-node 
    (key symbol?) 
    (left bintree?) 
    (right bintree?)))

(define tree-sum (lambda (tree)
  (cases bintree tree
    (leaf-node (num)
      (list 'leaf-node num))
    (interior-node (key left right)
      (list key (+ (cadr (tree-sum left)) (cadr (tree-sum right))))))))

(define max-interior (lambda (tree)
  (cases bintree tree
    (leaf-node (num)
      'none)
    (interior-node (key left right)
      (let ([left-sum (tree-sum left)]
            [right-sum (tree-sum right)])
        (cond [(> (cadr left-sum) (cadr right-sum)) (car left-sum)]
              [else (car right-sum)]
            ))))))

(define tree-1
        (interior-node 'foo (leaf-node 2) (leaf-node 3)))

(define tree-2
        (interior-node 'bar (leaf-node -1) tree-1))

(define tree-3
        (interior-node 'baz tree-2 (leaf-node 1)))

;; foo
(display (max-interior tree-2))

;; baz
(display (max-interior tree-3))
