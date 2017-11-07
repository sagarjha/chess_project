#lang racket

(require "chess.ss")
(require "positions.ss")
(provide (all-defined-out))
(require "minimax.ss")

(define chess%
  (class object%
    (super-new)
    (init (complete-position initial-complete-position))
    (define move-list (list))
    (define object-position-list (list))
;    (define mychessboard (new chess-gui%))
;    (send mychessboard show)
    (define chess-board (new board% (complete-board-position complete-position)))
    (define num-moves 0.0)
    
    (define/public (get-turn)
      (send chess-board flip (send chess-board get-turn)))
    
    (define/public (get-object-position-list)
      (map (lambda (complete-position)
             (get-field board-position complete-position))
           object-position-list))
    
    (define/public (get-move-list)
      (car move-list))
    
;    (define (wait-for-human-move)
;      (if (not (eq? (send mychessboard get-human-move) #f))
;          (void)
;          (wait-for-human-move)))
    
    (define/public (computer-play)
      (if (send chess-board drawn?)
	       (begin
		 (display "Draw by insufficient material")
                 (newline))
               (let* ([possible-complete-positions (send chess-board give-all-positions)]
                      [choice (if (not (null? possible-complete-positions))
                                  (random (length possible-complete-positions))
                                  -1)]
                      [chosen-position (if (not (eq? choice -1))
                                           ;(list-ref possible-complete-positions choice)
                                           (begin
                                             (display (get-best-move (get-field complete-board-position chess-board)))
                                             (newline)
                                             (get-best-move (get-field complete-board-position chess-board)))
                                           (list))]
                      [move (if (not (eq? choice -1))
                                (get-field move chosen-position)
                                (list))])
                 (begin
                   (set-field! turn chosen-position (send chess-board flip (send chess-board get-turn)))
                   (if (not (null? possible-complete-positions))
                       (begin
                         
                         (set-field! complete-board-position chess-board chosen-position)
                         (set! object-position-list (cons chosen-position object-position-list))
                         (set! move-list (cons move move-list))
                         (set! num-moves (+ num-moves 0.5))
                         ;(send mychessboard update-posn (cons (send chess-board flip (send chess-board get-turn)) move))
                         (display "completed update-posn")
                         (newline)
                         
                         ;(computer-play (cons (send chess-board get-move) move-list)
                         ;              (cons chosen-position object-position-list))
                         ;                (human-play (cons (send chess-board get-move) move-list)
                         ;                            (cons (car possible-complete-positions) object-position-list))
                         )
                       (begin
                         (if (send chess-board king-in-check? (send chess-board get-board-position))
			     (begin
			       (display "checkmate")
			       (newline))
			     (begin
			       (display "stalemate")
                               (newline)))
                         (display "game complete")
                         (newline)
                         (display num-moves)
                         (newline)
                         (display (reverse move-list))
                         (newline)
                         (reverse object-position-list)))))))
  
  (define/public (human-play candidate-move)
    
    (define refined-cadidate-move
      (cond [(and (equal? candidate-move (list "K" (cons 5 1) (cons 7 1)))
                  (eq? (send chess-board get-turn) "white"))
             (set! candidate-move (list "O-O"))]
            [(and (equal? candidate-move (list "K" (cons 5 1) (cons 3 1)))
                  (eq? (send chess-board get-turn) "white"))
             (set! candidate-move (list "O-O-O"))]
            [(and (equal? candidate-move (list "K" (cons 5 8) (cons 7 8)))
                  (eq? (send chess-board get-turn) "black"))
             (set! candidate-move (list "O-O"))]
            [(and (equal? candidate-move (list "K" (cons 5 8) (cons 3 8)))
                  (eq? (send chess-board get-turn) "black"))
             (set! candidate-move (list "O-O-O"))]))
    
    (if (send chess-board drawn?)
        (begin
          (display "Draw by insufficient material")
          (newline))
        (let* ([possible-complete-positions (send chess-board give-all-positions)]
               [check-if-valid (assoc candidate-move (map (lambda (board-pos) (list (get-field move board-pos) board-pos))
                                                          possible-complete-positions))])
          (begin
            (if (null? possible-complete-positions)
                (begin
                  (if (send chess-board king-in-check? (get-field complete-board-position chess-board))
                      (begin
                        (display "checkmate")
                        (newline))
                      (begin
                        (display "stalemate")
                        (newline)))
                  (display "game complete")
                  (newline)
                  (display num-moves)
                  (newline)
                  (display (reverse move-list))
                  (newline)
                  (reverse object-position-list))
                (if check-if-valid
                    (begin
                      (set! object-position-list (cons (cadr check-if-valid)  object-position-list))
                      (set-field! complete-board-position chess-board (cadr check-if-valid))
                      (set! move-list (cons candidate-move move-list))
                      (set! num-moves (+ num-moves 0.5))
                      ;(send mychessboard update-posn (cons (send chess-board flip (send chess-board get-turn)) move))
                      (set! num-moves (+ num-moves 0.5))
                      ;(send mychessboard set-was-valid 1)
                      ;                    (computer-play (cons candidate-move move-list) 
                      ;                                   (cons (car check-if-valid)
                      ;                                         object-position-list))
                      #t
                      )
                    (begin
                      (display "Invalid Move")
                      (newline)
                      #f
                      ;(send mychessboard set-was-valid! 2)
                      ;(human-play move-list object-position-list)
                      )))))))
    
    ;  (define/public (start-game)
    ;    (computer-play (list) (list)))
    ))
;
;(define chess-game (new chess% (complete-position initial-complete-position)))
;(define obj-list (send chess-game start-game))