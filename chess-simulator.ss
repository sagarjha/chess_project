#lang racket

(require "chess.ss")
(require "positions.ss")

(define chess%
  (class object%
	 (super-new)
	 (init (complete-position initial-complete-position))
	 (define chess-board (new board% (complete-board-position complete-position)))
	 (define num-moves 0.0)

	 ;; (define (flip turn)
	 ;;   (if (eq? turn 'white)
	 ;;       'black 'white))

	 (define/public (computer-play move-list object-position-list)
	   (let* ([possible-complete-positions (send chess-board give-all-positions)])
	     (begin
	       (send chess-board print)
	       (if (not (null? possible-complete-positions))
		   (begin
		     (set-field! complete-board-position chess-board (car possible-complete-positions))
		     (set! num-moves (+ num-moves 0.5))
		     (computer-play (cons (send chess-board get-move) move-list)
				    (cons (car possible-complete-positions) object-position-list)))
		   (begin
		     (display "game complete")
		     (newline)
		     (display num-moves)
		     (newline)
		     (display (reverse move-list))
		     (newline)
		     (reverse object-position-list))))))

	 ;; (define/public (human-play turn)
	 ;;   (begin
	 ;;     (let* ([move (read)]
	 ;; 	    [board-position (send chess-board get-board-position)]
         ;;            [half-position (if (eq? turn 'white)
         ;;                               (car board-position)
         ;;                               (cdr board-position))]
	 ;; 	    [other-half-position (if (eq? turn 'white)
	 ;; 				     (cdr board-position)
	 ;; 				     (car board-position))]
         ;;            [new-half-position (send chess-board change-pos half-position (first move) (second move) (third move))]
	 ;; 	    [new-other-half-position (send chess-board capture other-half-position (third move))])
	 ;;       (begin
	 ;; 	 (display new-other-half-position)
         ;;         (if (eq? turn 'white)
	 ;; 	     (send chess-board change (cons new-half-position new-other-half-position))
	 ;; 	     (send chess-board change (cons new-other-half-position new-half-position)))
	 ;; 	 (computer-play (flip turn))))))
	 
	 (define/public (start-game)
	   (computer-play (list) (list)))))

(define chess-game (new chess%))
(define obj-list (send chess-game start-game))