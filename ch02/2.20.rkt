#lang racket
#|
Exercise 2.20 [***] In the representation of binary trees in exercise 2.19 it is easy to move from 
  a parent node to one of its sons, but it is impossible to move from a son to its parent without 
  the help of context arguments. Extend the representation of lists in exercise 2.18 to 
  represent nodes in a binary tree. 
  
  As a hint, consider representing the portion of the tree above the current node 
  by a reversed list, as in exercise 2.18.

  In this representation, implement the procedures from exercise 2.19. 
  Also implement move-up, at-root?, and at-leaf?.
|#