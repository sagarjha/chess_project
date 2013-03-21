#lang racket

(define (find-king l)
  (

(define (detect-check l move)
  (let* ([player-one-position (if (= move 0) (car l) (cdr l))]
         [player-two-position (if (= move 0) (cdr l) (car l))]) ;detects if the player1 is in check
    ;find the position of the king of player one
    (begin
      (define king-pos (find-king player-one-position))