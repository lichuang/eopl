#lang racket
;; some notes about list

(define seq '(6 (5 4 3 2 1) (7 8 9)))

;; 5
(caadr seq)

;;'(4 3 2 1)
(cdadr seq)

;;'(7 8 9)
(caddr seq)

;; '(8 9)
(cdaddr seq)