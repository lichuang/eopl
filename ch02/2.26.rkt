#lang eopl

#|
Exercise 2.26 [**] Here is another version of exercise 1.33. 
Consider a set of trees given by the following grammar:

    Red-blue-tree ::= Red-blue-subtree

    Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
    ::= (blue-node {Red-blue-subtree}∗)
    ::= (leaf-node Int)

Write an equivalent definition using define-datatype, and use the resulting interface to 
  write a procedure that takes a tree and builds a tree of the same shape, 
  except that each leaf node is replaced by a leaf node that contains the number of 
  red nodes on the path between it and the root.
|#

;;todo
