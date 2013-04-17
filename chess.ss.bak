#lang racket

(require "lc.ss")
(require "2D-vector.ss")
(require "list-functions.ss")
(require "positions.ss")

(define chess-piece%
  (class object%
	 (super-new)
	 (define/public (resolvable? half-position new-pos)
	   (foldr (lambda (val pred) (if pred
					 (foldr (lambda (val-one pred-one)
						  (and pred-one (not (equal? val-one new-pos))))
						#t (cdr val))
					 #f))
		  #t half-position))
	 
	 (define/public (filter-posn pair-position lst)
	   (take-while (lambda (new-move) 
			 (resolvable? (car pair-position) new-move))
		       (lambda (new-move)
			 (resolvable? (cdr pair-position) new-move))
		       lst))))

(define pawn%
  (class chess-piece%
	 (super-new)
	 (define/public (valid? board-position move pos new-pos)
	   (if (member new-pos (give-all-moves board-position move pos))
	       #t #f))
	 
	 (define/public (give-all-moves board-position move pos)
	   (let* ([file (car pos)]
		  [rank (cdr pos)]
		  [pair-position (if (eq? move 'white)
				     board-position
				     (cons (cdr board-position) (car board-position)))])
	     (cond [(eq? move 'white)
		    (let* ([one-step-ahead (if (or (= rank 8) 
						   (not (send this resolvable? (car pair-position) (cons file (+ rank 1))))
						   (not (send this resolvable? (cdr pair-position) (cons file (+ rank 1)))))
					       #f (cons file (+ rank 1)))]
			   [two-steps-ahead (if (and (send this resolvable? (car pair-position) (cons file (+ rank 2)))
						     (send this resolvable? (cdr pair-position) (cons file (+ rank 2)))
						     (= rank 2) one-step-ahead)
						(cons file (+ rank 2)) #f)]
			   [capture-left (cond [(and (not (send this resolvable? (cdr pair-position) (cons (- file 1) (+ rank 1))))
						     (not (= file 1)) (not (= rank 8)))
						(cons (- file 1) (+ rank 1))]
					       [else #f])]
			   [capture-right (cond [(and (not (send this resolvable? (cdr pair-position) (cons (+ file 1) (+ rank 1))))
						      (not (= file 8)) (not (= rank 8)))
						 (cons (+ file 1) (+ rank 1))]
						[else #f])])
		      (remove-val #f (list one-step-ahead two-steps-ahead capture-left capture-right)))]
		   [(eq? move 'black)
		    (let* ([one-step-ahead (if (or (= rank 1)
						   (not (send this resolvable? (car pair-position) (cons file (- rank 1))))
						   (not (send this resolvable? (cdr pair-position) (cons file (- rank 1)))))
					       #f (cons file (- rank 1)))]
			   [two-steps-ahead (if (and (send this resolvable? (car pair-position) (cons file (- rank 2)))
						     (send this resolvable? (cdr pair-position) (cons file (- rank 2)))
						     (= rank 7) one-step-ahead)
						(cons file (- rank 2)) #f)]
			   [capture-left (cond [(and (not (send this resolvable? (cdr pair-position) (cons (- file 1) (- rank 1))))
						     (not (= file 1)) (not (= rank 1)))
						(cons (- file 1) (- rank 1))]
					       [else #f])]
			   [capture-right (cond [(and (not (send this resolvable? (cdr pair-position) (cons (+ file 1) (- rank 1))))
						      (not (= file 8)) (not (= rank 1)))
						 (cons (+ file 1) (- rank 1))]
						[else #f])])
		      (remove-val #f (list one-step-ahead two-steps-ahead capture-left capture-right)))])))))

(define knight%
  (class chess-piece%
	 (super-new)
	 (define/public (valid? board-position move pos new-pos)
	   (if (member new-pos (give-all-moves board-position move pos))
	       #t #f))

	 (define/public (give-all-moves board-position move pos)
	   (let* ([file (car pos)]
		  [rank (cdr pos)]
		  [half-position (if (eq? move 'white)
				     (car board-position)
				     (cdr board-position))])
	     (filter (lambda (new-pos) (send this resolvable? half-position new-pos))
		     (append (lc (cons x y) : x <- (list (- file 1) (+ file 1)) 
				 y <- (list (- rank 2) (+ rank 2))
				 * (> x 0) * (> y 0) * ( < x 9) * (< y 9))
			     (lc (cons x y) : x <- (list (- file 2) (+ file 2))
				 y <- (list (- rank 1) (+ rank 1))
				 * (> x 0) * (> y 0) * ( < x 9) * (< y 9))))))))

(define bishop%
  (class chess-piece%
	 (super-new)
	 (define/public (valid? board-position move pos new-pos)
	   (if (member new-pos (give-all-moves board-position move pos))
	       #t #f))

	 (define/public (give-all-moves board-position move pos)
	   (let* ([file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range 1 8)]
		  [pair-position (if (eq? move 'white)
				     board-position
				     (cons (cdr board-position) (car board-position)))])
	     (append 
	      (send this filter-posn pair-position
		    (lc (cons (+ file x) (+ rank x)) : x <- move-range 
			* (> (+ file x) 0) * (> (+ rank x) 0) 
			* (< (+ file x) 9) * (< (+ rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (- file x) (- rank x)) : x <- move-range 
			* (> (- file x) 0) * (> (- rank x) 0) 
			* (< (- file x) 9) * (< (- rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (+ file x) (- rank x)) : x <- move-range 
			* (> (+ file x) 0) * (> (- rank x) 0) 
			* (< (+ file x) 9) * (< (- rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (- file x) (+ rank x)) : x <- move-range 
			* (> (- file x) 0) * (> (+ rank x) 0)
			* (< (- file x) 9) * (< (+ rank x) 9))))))))

(define rook%
  (class chess-piece%
	 (super-new)
	 (define/public (valid? board-position move pos new-pos)
	   (if (member new-pos (give-all-moves board-position move pos))
	       #t #f))

	 (define/public (give-all-moves board-position move pos)
	   (let* ([file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range 1 8)]
		  [pair-position (if (eq? move 'white)
				     board-position
				     (cons (cdr board-position) (car board-position)))])
	     (append 
	      (send this filter-posn pair-position
		    (lc (cons (+ file x) rank) : x <- move-range 
			* (> (+ file x) 0) * (< (+ file x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (- file x) rank) : x <- move-range 
			* (> (- file x) 0) * (< (- file x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons file (+ rank x)) : x <- move-range 
			* (> (+ rank x) 0) * (< (+ rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons file (- rank x)) : x <- move-range 
			* (> (- rank x) 0) * (< (- rank x) 9))))))))

(define queen%
  (class chess-piece%
	 (super-new)
	 (define/public (valid? board-position move pos new-pos)
	   (if (member new-pos (give-all-moves board-position move pos))
	       #t #f))

	 (define/public (give-all-moves board-position move pos)
	   (let* ([file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range 1 8)]
		  [pair-position (if (eq? move 'white)
				     board-position
				     (cons (cdr board-position) (car board-position)))])
	     (append 
	      (send this filter-posn pair-position
		    (lc (cons (+ file x) (+ rank x)) : x <- move-range 
			* (> (+ file x) 0) * (> (+ rank x) 0) 
			* (< (+ file x) 9) * (< (+ rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (- file x) (- rank x)) : x <- move-range 
			* (> (- file x) 0) * (> (- rank x) 0) 
			* (< (- file x) 9) * (< (- rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (+ file x) (- rank x)) : x <- move-range 
			* (> (+ file x) 0) * (> (- rank x) 0) 
			* (< (+ file x) 9) * (< (- rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (- file x) (+ rank x)) : x <- move-range 
			* (> (- file x) 0) * (> (+ rank x) 0)
			* (< (- file x) 9) * (< (+ rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (+ file x) rank) : x <- move-range 
			* (> (+ file x) 0) * (< (+ file x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons (- file x) rank) : x <- move-range 
			* (> (- file x) 0) * (< (- file x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons file (+ rank x)) : x <- move-range 
			* (> (+ rank x) 0) * (< (+ rank x) 9)))
	      (send this filter-posn pair-position
		    (lc (cons file (- rank x)) : x <- move-range 
			* (> (- rank x) 0) * (< (- rank x) 9))))))))

(define king%
  (class chess-piece%
	 (super-new)
	 (define/public (valid? board-position move pos new-pos)
	   (if (member new-pos (give-all-moves board-position move pos))
	       #t #f))

	 (define/public (give-all-moves board-position move pos)
	   (let* ([file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)]
		  [half-position (if (eq? move 'white)
				     (car board-position)
				     (cdr board-position))])
	     (filter (lambda (new-pos) (send this resolvable? half-position new-pos))
		     (append (lc (cons x y) : x <- (list (- file 1) file (+ file 1))
				 y <- (list (- rank 1) rank (+ rank 1))
				 * (> x 0) * (< x 9) * (> y 0) * (< y 9)
				 * (or (not (= x file)) (not (= y file))))))))))

(define board%
  (class object%
	 (super-new)
	 (init-field (complete-board-position initial-complete-position))
	 (define board-position (get-field board-position complete-board-position))
	 (define move (get-field move complete-board-position))
	 
	 (define dummy-pawn (make-object pawn%))
	 (define dummy-knight (make-object knight%))
	 (define dummy-rook (make-object rook%))
	 (define dummy-bishop (make-object bishop%))
	 (define dummy-queen (make-object queen%))
	 (define dummy-king (make-object king%))
	 
	 (define short-castle-enable-white (get-field scew complete-board-position))
	 (define short-castle-enable-black (get-field sceb complete-board-position))
	 (define long-castle-enable-white (get-field lcew complete-board-position))
	 (define long-castle-enable-black (get-field lceb complete-board-position))

	 (define/public (read-castle-enable num)
	   (cond [(eq? num 0) short-castle-enable-white]
		 [(eq? num 1) short-castle-enable-black]
		 [(eq? num 2) long-castle-enable-white]
		 [(eq? num 3) long-castle-enable-black]))

	 (define/public (castle-disable num)
	   (cond [(eq? num 0) (set! short-castle-enable-white #f)]
		 [(eq? num 1) (set! short-castle-enable-black #f)]
		 [(eq? num 2) (set! long-castle-enable-white #f)]
		 [(eq? num 3) (set! long-castle-enable-black #f)]))

         (define (flip move)
           (if (eq? move 'white)
               'black
               'white))
	 
	 ;; (define/public (setup)
	 ;;   (define (setup-one-class position-one-type side)
	 ;;     (foldr (lambda (val lst) (2d-vector-set! chess-board (- (car val) 1) (- (cdr val) 1) 
	 ;; 					      (cons side (car position-one-type)))) 
	 ;; 	    (list) (cdr position-one-type)))
	 ;;   (define (setup-white half-position)
	 ;;     (foldr (lambda (val lst) (setup-one-class val 'white)) (list) half-position))
	 ;;   (define (setup-black half-position)
	 ;;     (foldr (lambda (val lst) (setup-one-class val 'black)) (list) half-position))
	 ;;   (begin
	 ;;     (setup-white (car board-position))
	 ;;     (setup-black (cdr board-position))
	 ;;     (print)))

	 ;; (setup)

	 (define (get-dummy symbol)
	   (cond [(eq? symbol "P") dummy-pawn]
		 [(eq? symbol "N") dummy-knight]
		 [(eq? symbol "R") dummy-rook]
		 [(eq? symbol "B") dummy-bishop]
		 [(eq? symbol "Q") dummy-queen]
		 [(eq? symbol "K") dummy-king]))

	 (define (promote lst old-pos pos piece)
	   (add-to-list (capture lst old-pos) piece pos))

	 (define (add-to-list lst piece pos)
	   (let* ([piece-set (map (lambda (x) (car x)) lst)])
	     (if (member piece piece-set)
		 (foldr (lambda (val l)
			  (if (eq? (car val) piece)
			      (cons (cons (car val) (cons pos (cdr val))) l)
			      (cons val l)))
			(list) lst)
		 (cons (list piece pos) lst))))
						     
	 (define/public (change-pos half-position piece init-pos pos)
	   (define (change-pos-h lst init-pos pos)
	     (if (equal? (car lst) init-pos)
		 (cons pos (cdr lst))
		 (cons (car lst) (change-pos-h (cdr lst) init-pos pos))))
	   (if (eq? (car (car half-position)) piece)
	       (let* ([changed-list (change-pos-h (cdr (car half-position)) init-pos pos)])
		 (cons (cons piece changed-list) (cdr half-position)))
	       (cons (car half-position) (change-pos (cdr half-position) piece init-pos pos))))

	 (define/public (capture other-half-position pos)
	   (define (capture-h lst)
	     (cond [(null? lst) lst]
		   [(equal? (car lst) pos) (cdr lst)]
		   [else (cons (car lst) (capture-h (cdr lst)))]))
           (if (null? other-half-position)
               (list)
               (let* ([captured-lst (capture-h (cdr (car other-half-position)))])
		 (if (null? captured-lst)
		     (capture (cdr other-half-position) pos)
		     (cons (cons (caar other-half-position) captured-lst)
			   (capture (cdr other-half-position) pos))))))

	 (define (process piece-set half-position other-half-position)
	   (foldr (lambda (pos pos-lst) 
		    (append (foldr (lambda (val lst) 
				     (if (eq? move 'white)
					 (if (and (eq? (car piece-set) "P") (= (cdr val) 8))
					     (append (cons (cons (promote half-position pos val "Q")
								 (capture other-half-position val))
							   (list (cons (promote half-position pos val "N")
								       (capture other-half-position val)))) lst)
					     (cons (cons (change-pos half-position (car piece-set) pos val) 
							 (capture other-half-position val)) lst))
					 (if (and (eq? (car piece-set) "P") (= (cdr val) 1))
					     (append (cons (cons (capture other-half-position val)
								 (promote half-position pos val "Q"))
							   (list (cons (capture other-half-position val)
								       (promote half-position pos val "N")))) lst)
					     (cons (cons (capture other-half-position val)
							 (change-pos half-position (car piece-set) pos val)) lst))))
				   (list) (send (get-dummy (car piece-set)) give-all-moves board-position move pos)) pos-lst))
		  (list) (cdr piece-set)))

	 (define/public (capturable? enemy-position square)
	   (foldr (lambda (elem res-f)
		    (or (foldr (lambda (pos res)
				 (or (send (get-dummy (car elem)) valid? board-position (flip move) pos square) res))
			       #f (cdr elem)) res-f))
		  #f enemy-position))

	 (define/public (king-in-check? board-position)
	   (define (get-king-pos half-position)
	     (foldr (lambda (elem found-pos)
		      (if found-pos found-pos
			  (if (eq? (first elem) "K")
			      (second elem)
			      #f)))
		    #f half-position))
	   (let* ([half-position (if (eq? move 'white)
				     (car board-position)
				     (cdr board-position))]
		  [other-half-position (if (eq? move 'white)
					   (cdr board-position)
					   (car board-position))]
		  [king-pos (get-king-pos half-position)])
	     (foldr (lambda (elem res-f)
                      (or (foldr (lambda (pos res)
				   (or (send (get-dummy (car elem)) valid? board-position (flip move) pos king-pos) res))
				 #f (cdr elem)) res-f))
		    #f other-half-position)))

	 (define/public (give-castling-moves)
	   (let* ([dummy-piece (new chess-piece%)]
		  [rank (if (eq? move 'white)
			    1 8)]
		  [short-castle-enable (if (eq? move 'white)
					   short-castle-enable-white
					   short-castle-enable-black)]
		  [long-castle-enable (if (eq? move 'white)
					  long-castle-enable-white
					  long-castle-enable-black)]
		  [self-position (if (eq? move 'white)
				     (car board-position)
				     (cdr board-position))]
		  [enemy-position (if (eq? move 'white)
				      (cdr board-position)
				      (car board-position))])
	     (append
	      (if short-castle-enable
		  (if (and (send dummy-piece resolvable? self-position (cons 6 rank))
			   (send dummy-piece resolvable? self-position (cons 7 rank))
			   (send dummy-piece resolvable? enemy-position (cons 6 rank))
			   (send dummy-piece resolvable? enemy-position (cons 7 rank))
			   (not (capturable? enemy-position (cons 5 rank)))
			   (not (capturable? enemy-position (cons 6 rank)))
			   (not (capturable? enemy-position (cons 7 rank))))
		      (if (eq? move 'white)
			  (list (cons (change-pos (change-pos self-position "K" (cons 5 rank) (cons 7 rank))
					    "R" (cons 8 rank) (cons 6 rank))
				enemy-position))
			  (list (cons enemy-position
				(change-pos (change-pos self-position "K" (cons 5 rank) (cons 7 rank))
					    "R" (cons 8 rank) (cons 6 rank)))))
		      (list))
		  (list))
	      (if long-castle-enable
		  (if (and (send dummy-piece resolvable? self-position (cons 4 rank))
			   (send dummy-piece resolvable? self-position (cons 3 rank))
			   (send dummy-piece resolvable? self-position (cons 2 rank))
			   (send dummy-piece resolvable? enemy-position (cons 4 rank))
			   (send dummy-piece resolvable? enemy-position (cons 3 rank))
			   (send dummy-piece resolvable? enemy-position (cons 2 rank))
			   (not (capturable? enemy-position (cons 5 rank)))
			   (not (capturable? enemy-position (cons 4 rank)))
			   (not (capturable? enemy-position (cons 3 rank))))
		      (if (eq? move 'white)
			  (list (cons (change-pos (change-pos self-position "K" (cons 5 rank) (cons 3 rank))
					    "R" (cons 1 rank) (cons 4 rank))
				enemy-position))
			  (list (cons enemy-position
				(change-pos (change-pos self-position "K" (cons 5 rank) (cons 3 rank))
					    "R" (cons 1 rank) (cons 4 rank)))))
		      (list))
		  (list)))))

	 (define/public (give-all-positions)
	   (let* ([half-position (if (eq? move 'white)
				     (car board-position)
				     (cdr board-position))]
		  [other-half-position (if (eq? move 'white)
					   (cdr board-position)
					   (car board-position))]
		  [castling-moves (give-castling-moves)])
	     (append castling-moves 
		   (filter (lambda (board-position)
			     (not (king-in-check? board-position)))
			   (foldr (lambda (val lst) (append (process val half-position other-half-position) lst))
				  (list) half-position)))))
	 
	 ;; (define/public (change board-pos)
	 ;;   (begin
	 ;;     (set! chess-board (make-2d-vector 8 8))
	 ;;     (set! board-position board-pos)
	 ;;     (setup)))

	 ;; (define/public (get-board-position)
	 ;;   board-position)

	 ;; (define/public (print)
	 ;;   (display chess-board)
	 ;;   (newline))
	 ))

;; (define chess%
;;   (class object%
;; 	 (super-new)
;; 	 (define chess-board (new board%))

;; 	 (define (flip turn)
;; 	   (if (eq? turn 'white)
;; 	       'black 'white))

;; 	 (define/public (computer-play turn board-position)
;; 	   (begin
;; 	     (let* ([possible-positions (send chess-board give-all-positions)])
;; 	       (begin
;; 		 (human-play (flip turn))))))

;; 	 (define/public (human-play turn)
;; 	   (begin
;; 	     (let* ([move (read)]
;; 		    [board-position (send chess-board get-board-position)]
;;                     [half-position (if (eq? turn 'white)
;;                                        (car board-position)
;;                                        (cdr board-position))]
;; 		    [other-half-position (if (eq? turn 'white)
;; 					     (cdr board-position)
;; 					     (car board-position))]
;;                     [new-half-position (send chess-board change-pos half-position (first move) (second move) (third move))]
;; 		    [new-other-half-position (send chess-board capture other-half-position (third move))])
;; 	       (begin
;; 		 (display new-other-half-position)
;;                  (if (eq? turn 'white)
;; 		     (send chess-board change (cons new-half-position new-other-half-position))
;; 		     (send chess-board change (cons new-other-half-position new-half-position)))
;; 		 (computer-play (flip turn))))))

;; 	 (computer-play 'white)))

(define B (new board% (complete-board-position random-complete-position-1)))
(send B give-all-positions)

;(define C (new chess%))