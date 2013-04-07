#lang racket

(require "lc.ss")
(require "initialPosition.ss")
(require "2D-vector.ss")
(require "list-functions.ss")

(define chess-piece%
  (class object%
	 (super-new)
	 (define status 0);
	 (init-field pos);
	 (define position pos);
	 (define/public (get-position)
	   (if (eq? status 0)
	       position
	       (error "The piece is not active")))
	 (define/public (resolvable? half-position new-pos)
	   (foldr (lambda (val pred) (if pred
					 (foldr (lambda (val-one pred-one)
						  (and pred-one (not (equal? val-one new-pos))))
						#t (cdr val))
					 #f))
		  #t half-position))
	 (define/public (set-position! pos)
	   (set! position pos))
	 (define/public (filter-posn pair-position lst)
	   (take-while (lambda (new-move) 
			 (resolvable? (car pair-position) new-move))
		       (lambda (new-move)
			 (resolvable? (cdr pair-position) new-move))
		       lst))))

(define pawn%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves board-position move)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
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
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves board-position move)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
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
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves board-position move)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
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
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves board-position move)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
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
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves board-position move)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
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
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves board-position move)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
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
	 (define board-position initial-position)
	 (define move 'white)
	 (define chess-board (make-2d-vector 8 8))
	 (define dummy-pos (cons 0 0))
	 (define dummy-pawn (make-object pawn% dummy-pos))
	 (define dummy-knight (make-object knight% dummy-pos))
	 (define dummy-rook (make-object rook% dummy-pos))
	 (define dummy-bishop (make-object bishop% dummy-pos))
	 (define dummy-queen (make-object queen% dummy-pos))
	 (define dummy-king (make-object king% dummy-pos))
	 
	 (define/public (setup)
	   (define (setup-one-class position-one-type side)
	     (foldr (lambda (val lst) (2d-vector-set! chess-board (- (car val) 1) (- (cdr val) 1) 
						      (cons side (car position-one-type)))) 
		    (list) (cdr position-one-type)))
	   (define (setup-white half-position)
	     (foldr (lambda (val lst) (setup-one-class val 'white)) (list) half-position))
	   (define (setup-black half-position)
	     (foldr (lambda (val lst) (setup-one-class val 'black)) (list) half-position))
	   (begin
	     (setup-white (car board-position))
	     (setup-black (cdr board-position))
	     (print)))

	 (define (get-dummy symbol pos)
	   (cond [(eq? symbol 'P) (begin
				    (send dummy-pawn set-position! pos)
				    dummy-pawn)]
		 [(eq? symbol 'N) (begin
				    (send dummy-knight set-position! pos)
				    dummy-knight)]
		 [(eq? symbol 'R) (begin
				    (send dummy-rook set-position! pos)
				    dummy-rook)]
		 [(eq? symbol 'B) (begin
				    (send dummy-bishop set-position! pos)
				    dummy-bishop)]
		 [(eq? symbol 'Q) (begin
				    (send dummy-queen set-position! pos)
				    dummy-queen)]
		 [(eq? symbol 'K) (begin
				    (send dummy-king set-position! pos)
				    dummy-king)]))

	 (define (change-pos board-pos piece init-pos pos)
	   (define (change-pos-h lst init-pos pos)
	     (if (equal? (car lst) init-pos)
		 (cons pos (cdr lst))
		 (cons (car lst) (change-pos-h (cdr lst) init-pos pos))))
	   (if (eq? (car (car board-pos)) piece)
	       (let* ([changed-list (change-pos-h (cdr (car board-pos)) init-pos pos)])
		 (cons (cons piece changed-list) (cdr board-pos)))
	       (cons (car board-pos) (change-pos (cdr board-pos) piece init-pos pos))))

	 (define (process piece-set half-position)
	   (foldr (lambda (pos pos-lst) 
		    (append (foldr (lambda (val lst) 
				     (cons (change-pos half-position (car piece-set) pos val) lst))
				   (list) (send (get-dummy (car piece-set) pos) give-all-moves board-position move)) pos-lst))
		  (list) (cdr piece-set)))

	 (define/public (give-all-positions)
	   (let* ([half-position (if (eq? move 'white)
				     (car board-position)
				     (cdr board-position))])
	     (if (eq? move 'white)
		 (map (lambda (x) (cons x (cdr board-position)))
		      (foldr (lambda (val lst) (append (process val half-position) lst))
			     (list) half-position))
		 (map (lambda (x) (cons (car board-position) x))
		      (foldr (lambda (val lst) (append (process val half-position) lst))
			     (list) half-position)))))
	 (define/public (print)
	   (display chess-board)
	   (newline))))

(define B (make-object board%))
(send B give-all-positions)
(newline)
(newline)
(newline)
(send B print)
(send B setup)