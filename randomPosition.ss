#lang racket

(provide random-position)

(define random-position
  (cons
   (list
    (list 'P (cons 4 4))
    (list 'K (cons 5 3)))
   (list
    (list 'P (cons 5 5))
    (list 'B (cons 1 7))
    (list 'K (cons 6 6))
    )
   ))