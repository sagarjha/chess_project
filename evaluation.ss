#lang racket

(require "2D-vector.ss")
(require "lc.ss")

(provide evaluate-posn)

;any-arbit-pos must be the complete position

(define any-arbit-pos (cons (list
                          (list "P" (cons 1 2) (cons 2 2) (cons 3 2) (cons 4 2) (cons 5 4) (cons 6 2) (cons 7 2) (cons 8 2))
                          (list "R" (cons 1 1) (cons 6 1))
                          (list "N" (cons 2 1) (cons 6 3))
                          (list "B" (cons 3 1) (cons 2 5))
                          (list "Q" (cons 4 1))
                          (list "K" (cons 7 1)))
                         (list 
                          (list "P" (cons 1 7) (cons 2 7) (cons 3 7) (cons 4 7) (cons 5 5) (cons 6 7) (cons 7 7) (cons 8 7))
                          (list "R" (cons 1 8) (cons 8 8))
                          (list "N" (cons 3 6) (cons 6 6))
                          (list "B" (cons 3 8) (cons 6 8))
                          (list "Q" (cons 4 8))
                          (list "K" (cons 5 8)))))

(define (evaluate-posn comp-posn) 2)

(struct piece (rank file type color) #:transparent)
(define fboard (make-2d-vector 8 8))

; AS IT STANDS, EVERY VALUE RETURNED WILL BE WHITE-BLACK

(define white-knights (cdr (car (lc x : x <- (car any-arbit-pos) * (equal? (car x) "N")))))
(define black-knights (cdr (car (lc x : x <- (cdr any-arbit-pos) * (equal? (car x) "N")))))
(define white-rooks (cdr (car (lc x : x <- (car any-arbit-pos) * (equal? (car x) "R")))))
(define black-rooks (cdr (car (lc x : x <- (cdr any-arbit-pos) * (equal? (car x) "R")))))
(define white-bishops (cdr (car (lc x : x <- (car any-arbit-pos) * (equal? (car x) "B")))))
(define black-bishops (cdr (car (lc x : x <- (cdr any-arbit-pos) * (equal? (car x) "B")))))
(define white-pawns (cdr (car (lc x : x <- (car any-arbit-pos) * (equal? (car x) "P")))))
(define black-pawns (cdr (car (lc x : x <- (cdr any-arbit-pos) * (equal? (car x) "P")))))
(define white-queens (cdr (car (lc x : x <- (car any-arbit-pos) * (equal? (car x) "Q")))))
(define black-queens (cdr (car (lc x : x <- (cdr any-arbit-pos) * (equal? (car x) "Q")))))
(define white-king (car (cdr (car (lc x : x <- (car any-arbit-pos) * (equal? (car x) "K"))))))
(define black-king (car (cdr (car (lc x : x <- (cdr any-arbit-pos) * (equal? (car x) "K"))))))
;The king variables are singletons. All the others are lists.

(define (place-pieces list type color)
  (foldr (lambda (cr anscdr)
           (+ anscdr (begin (2d-vector-set! fboard (- (car cr) 1) (- (cdr cr) 1) (piece (car cr) (cdr cr) type color)) 0))) 0 list))

(define (place-all-pieces) 
  (place-pieces white-knights "N" 'white) (place-pieces black-knights "N" 'black)
  (place-pieces white-rooks "R" 'white) (place-pieces black-rooks "R" 'black)
  (place-pieces white-bishops "B" 'white) (place-pieces black-bishops "B" 'black)
  (place-pieces white-queens "Q" 'white) (place-pieces black-queens "Q" 'black)
  (place-pieces white-pawns "P" 'white) (place-pieces black-pawns "P" 'black)
  (place-pieces (list white-king) "K" 'white) (place-pieces (list black-king) "K" 'black))

(place-all-pieces) (display "Pieces placed")

(define one-side-material-at-start 4140)
;TOTAL MATERIAL WEIGHT OF ALL PIECES (ONE COLOR ONLY) EQUALS 4140

(define material-knights (* 330 (- (length white-knights) (length black-knights))))
;MATERIAL VALUE DIFFERENCE (W - B) FOR KNIGHTS

(define material-bishops (* 330 (- (length white-bishops) (length black-bishops))))
;MATERIAL VALUE DIFFERENCE (W - B) FOR BISHOPS

(define material-rooks (* 520 (- (length white-rooks) (length black-rooks))))
;MATERIAL VALUE DIFFERENCE (W - B) FOR ROOKS

(define material-queens (* 980 (- (length white-queens) (length black-queens))))
;MATERIAL VALUE DIFFERENCE (W - B) FOR QUEENS

(define material-pawns (* 100 (- (length white-pawns) (length black-pawns))))  
;MATERIAL VALUE DIFFERENCE (W - B) FOR PAWNS

(define net-white-material
  (+ (* 330 (length white-knights)) (* 330 (length white-bishops)) (* 520 (length white-rooks)) (* 980 (length white-queens)) (* 100 (length white-pawns))))

(define net-black-material
  (+ (* 330 (length black-knights)) (* 330 (length black-bishops)) (* 520 (length black-rooks)) (* 980 (length black-queens)) (* 100 (length black-pawns))))

(define net-total-material
  (- net-white-material net-black-material))

(define (knights-close-to-center)
  (define (it-over-knights k-list)
    (foldr (lambda (x y) 
             (+ y (let* ([file (car x)]
                         [rank (cdr x)]) 
                    (min (+ (abs (- 4 file)) (abs (- 4 rank))) 
                         (+ (abs (- 5 file)) (abs (- 4 rank))) 
                         (+ (abs (- 4 file)) (abs (- 5 rank))) 
                         (+ (abs (- 5 file)) (abs (- 5 rank))))))) 0 k-list))
    (* (/ 5 4) (- (it-over-knights black-knights) (it-over-knights white-knights))))
;MEASURE OF PROXIMITY OF KNIGHT TO THE CENTER- CLOSER THE BETTER

(define (queen-enemy-king-taxicab-distance)
  (define (it-over-queens q-list king)
    (foldr (lambda (x y)
             (+ y (abs (- (car x) (car king))) (abs (- (cdr x) (cdr king))))) 0 q-list))
  (/ (- (it-over-queens white-queens black-king) (it-over-queens black-queens white-king)) 2))
;QUEENS ARE PENALIZED FOR TAXICAB DISTANCE TO THE ENEMY KING

(define (rook-enemy-king-taxicab-distance)
  (define (it-over-rooks r-list king)
    (foldr (lambda (x y)
             (+ y (abs (- (car x) (car king))) (abs (- (cdr x) (cdr king))))) 0 r-list))
  (/ (- (it-over-rooks white-rooks black-king) (it-over-rooks black-rooks white-king)) 2))
;ROOKS ARE PENALIZED FOR TAXICAB DISTANCE TO THE ENEMY KING

(define (knights-radius-of-two)
  (define (one-knight posn type)
    (let* ([k-file (car posn)]
           [k-rank (cdr posn)]
           [indices (lc (cons x y): x <- (range (max 1 (- k-file 2)) (min 9 (+ k-file 3))) y <- (range (max 1 (- k-rank 2)) (min 9 (+ k-rank 3)))
                        * (if (equal? #f (2d-vector-ref fboard (- x 1) (- y 1))) 
                              #f 
                              (not (equal? (piece-color (2d-vector-ref fboard (- x 1) (- y 1))) type))))])
      (length indices)))
  (define (all-knights k-list type)
    (foldr (lambda (x y) (+ y (one-knight x type))) 0 k-list))
  (let* ([percent-endgame (- 1 (min (/ net-white-material one-side-material-at-start) (/ net-black-material one-side-material-at-start)))])
    (* 4 percent-endgame (- (all-knights white-knights 'white) (all-knights black-knights 'black)))))

;(define (knights-not-driven-away)
