#lang racket

(provide initial-position)

(define initial-position
  (cons
   (list
    (list 'P (cons 1 2) (cons 2 2) (cons 3 2) (cons 4 2) (cons 5 2) (cons 6 2) (cons 7 2) (cons 8 2))
    (list 'R (cons 1 1) (cons 8 1))
    (list 'N (cons 2 1) (cons 7 1))
    (list 'B (cons 3 1) (cons 6 1))
    (list 'Q (cons 4 1))
    (list 'K (cons 5 1)))
   (list
    (list 'P (cons 1 7) (cons 2 7) (cons 3 7) (cons 4 7) (cons 5 7) (cons 6 7) (cons 7 7) (cons 8 7))
    (list 'R (cons 1 8) (cons 8 8))
    (list 'N (cons 2 8) (cons 7 8))
    (list 'B (cons 3 8) (cons 6 8))
    (list 'Q (cons 4 8))
    (list 'K (cons 5 8))
    )
   ))