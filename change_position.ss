#lang racket

(require "initialPosition.ss")

(define move 'white)

(define board-pos (if (eq? move 'white)
			(car initial-position)
			(cdr initial-position)))

(define (change-pos board-pos piece init-pos pos)
  (define (change-pos-h lst init-pos pos)
    (if (equal? (car lst) init-pos)
	(cons pos (cdr lst))
	(cons (car lst) (change-pos-h (cdr lst) init-pos pos))))
  (if (eq? (car (car board-pos)) piece)
      (let* ([changed-list (change-pos-h (cdr (car board-pos)) init-pos pos)])
	  (cons (cons piece changed-list) (cdr board-pos)))
      (cons (car board-pos) (change-pos (cdr board-pos) piece init-pos pos))))