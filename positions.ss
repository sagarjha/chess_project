#lang racket

(provide (all-defined-out))

(define complete-position%
  (class object%
	 (super-new)
	 (init-field board-position)
	 (init-field turn)
	 (init-field move)
	 (init-field scew)
	 (init-field sceb)
	 (init-field lcew)
	 (init-field lceb)
	 (define/public (print)
	   (begin
	     (display board-position)
	     (newline)
	     (display turn)
	     (newline)
	     (display move)
	     (newline)
	     (display scew)
	     (newline)
	     (display sceb)
	     (newline)
	     (display lcew)
	     (newline)
	     (display lceb)
	     (newline)))))

(define random-position
  (cons
   (list
    (list "P" (cons 3 7))
    (list "Q" (cons 4 1))
    (list "K" (cons 4 8)))
   (list
    (list "K" (cons 5 5))
    (list "P" (cons 8 2) (cons 4 4))
    )
   ))

(define initial-position
  (cons
   (list
    (list "P" (cons 1 2) (cons 2 2) (cons 3 2) (cons 4 2) (cons 5 2) (cons 6 2) (cons 7 2) (cons 8 2))
    (list "R" (cons 1 1) (cons 8 1))
    (list "N" (cons 2 1) (cons 7 1))
    (list "B" (cons 3 1) (cons 6 1))
    (list "Q" (cons 4 1))
    (list "K" (cons 5 1)))
   (list
    (list "P" (cons 1 7) (cons 2 7) (cons 3 7) (cons 4 7) (cons 5 7) (cons 6 7) (cons 7 7) (cons 8 7))
    (list "R" (cons 1 8) (cons 8 8))
    (list "N" (cons 2 8) (cons 7 8))
    (list "B" (cons 3 8) (cons 6 8))
    (list "Q" (cons 4 8))
    (list "K" (cons 5 8))
    )
   ))

(define random-position-1
  (cons
   (list
    (list "P" (cons 1 2) (cons 2 2) (cons 3 2) (cons 4 4) (cons 5 5) (cons 6 2) (cons 7 2) (cons 8 2))
    (list "R" (cons 1 1) (cons 8 1))
    (list "N" (cons 3 3) (cons 6 3))
    (list "B" (cons 3 1) (cons 2 5))
    (list "Q" (cons 4 1))
    (list "K" (cons 5 1)))
   (list
    (list "N" (cons 3 6) (cons 5 7))
    (list "P" (cons 1 7) (cons 2 7) (cons 3 7) (cons 4 5) (cons 5 6) (cons 6 7) (cons 7 7) (cons 8 7))
    (list "R" (cons 1 8) (cons 8 8))
    (list "B" (cons 3 8) (cons 2 4))
    (list "Q" (cons 4 8))
    (list "K" (cons 5 8))
    )
   ))

(define random-position-2
  (cons
   (list
    (list "P" (cons 5 7) (cons 5 2) (cons 6 2) (cons 7 2) (cons 8 2))
    (list "R" (cons 8 1))
    (list "N" (cons 2 1) (cons 7 1))
    (list "B" (cons 6 1))
    (list "Q" (cons 2 8) (cons 4 1))
    (list "K" (cons 5 1)))
   (list
    (list "N" (cons 7 8))
    (list "P" (cons 3 3) (cons 6 7) (cons 7 7) (cons 8 7))
    (list "R" (cons 1 8) (cons 8 8))
    (list "B" (cons 3 8) (cons 6 8))
    (list "Q" (cons 4 2) (cons 1 1) (cons 4 8))
    (list "K" (cons 4 7))
    )
   ))

(define initial-complete-position
  (make-object complete-position% initial-position 'white (cons 0 0) #t #t #t #t))

(define random-complete-position
  (make-object complete-position% random-position 'white (cons 0 0) #f #f #f #f))

(define random-complete-position-1
  (make-object complete-position% random-position-1 'black (cons 0 0) #t #t #t #t))

(define random-complete-position-2
  (make-object complete-position% random-position-2 'white (cons 0 0) #t #f #t #t))