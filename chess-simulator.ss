#lang racket

(require "chess_gui.ss")
(require "chess.ss")
(require "positions.ss")
(provide (all-defined-out))
(require "minimax.ss")

(define chess%
  (class object%
	 (super-new)
	 (init (complete-position initial-complete-position))
	 (define mychessboard (new chess-gui%))
	 (send mychessboard show)
	 (define chess-board (new board% (complete-board-position complete-position)))
	 (define num-moves 0.0)

	 (define/public (computer-play move-list object-position-list)
	   (display (get-field complete-board-position chess-board))
	   (newline)
	   (display "here")
	   (newline)
	   (read)
	   (let* ([possible-complete-positions (send chess-board give-all-positions)]
		  [choice (if (not (null? possible-complete-positions))
			      (random (length possible-complete-positions))
			      -1)]
		  [chosen-position (if (not (eq? choice -1))
				       ;(list-ref possible-complete-positions choice)
				       (get-best-move (get-field complete-board-position chess-board))
				       (list))]
		  [move (if (not (eq? choice -1))
			    (get-field move chosen-position)
			    (list))])
	     (begin
	       (send chess-board print)
	       (if (not (null? possible-complete-positions))
		   (begin
		     (set-field! complete-board-position chess-board chosen-position)
		     (set! num-moves (+ num-moves 0.5))
		     (send mychessboard update-posn (cons (send chess-board get-turn) move))
		     ;(human-play (cons (send chess-board get-move) move-list)
		;		 (cons (car possible-complete-positions) object-position-list)))
		     )
		   (begin
		     (display "game complete")
		     (newline)
		     (display num-moves)
		     (newline)
		     (display (reverse move-list))
		     (newline)
		     (reverse object-position-list))))))
	 
	 (define/public (human-play move-list object-position-list)
	   (begin
	     (send chess-board print)
	     (let* ([move (read)]
	 	    [possible-complete-positions (send chess-board give-all-positions)]
		    [check-if-valid (assoc move (map (lambda (board-pos) (list (get-field move board-pos) board-pos))
						     possible-complete-positions))])
	       (begin
		 (if (null? possible-complete-positions)
		     (begin
		       (display "game complete")
		       (newline)
		       (display num-moves)
		       (newline)
		       (display (reverse move-list))
		       (newline)
		       (reverse object-position-list))
		     (if check-if-valid
			 (begin
			   (set-field! complete-board-position chess-board (cadr check-if-valid))
			   (send mychessboard update-posn (cons (send chess-board get-turn) move))
			   (set! num-moves (+ num-moves 0.5))
			   (computer-play (cons move move-list) 
					  (cons (car check-if-valid)
						object-position-list)))
			 (begin
			   (display "Invalid Move")
			   (newline)
			   (human-play move-list object-position-list))))))))
	 
	 (define/public (start-game)
	   (computer-play (list) (list)))))

(define chess-game (new chess% (complete-position initial-complete-position)))
(define obj-list (send chess-game start-game))