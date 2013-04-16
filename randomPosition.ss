#lang racket

(provide random-position
	 complete-position
	 random-complete-position)

(struct complete-position (board-position
			   move
			   short-castle-enable-white
			   short-castle-enable-black
			   long-castle-enable-white
			   long-castle-enable-black))

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

(define random-complete-position
  (complete-position random-position 'white #f #f #f #f))