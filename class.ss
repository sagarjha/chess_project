#lang racket

(require "lc.ss")
(require "initialPosition.ss")

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
	 (define/public (set-position! pos)
	   (set! position pos))))

(define pawn%
  (class chess-piece%
	 (inherit-field pos)
	 (define turn 'white)
	 (super-new)
	 (define/public (set-move! move)
	   (set! turn move))
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)])
	     (cond [(eq? turn 'white)
		    (if (not (eq? rank 2))
			(lc (cons x y) : x <- (list (- file 1) file (+ file 1))
			    y <- (list (+ rank 1)) * (> x 0) 
			    * (> y 0) * ( < x 9) * (< y 9))     
			(cons (cons file (+ rank 2)) 
			      (lc (cons x y) : x <- (list (- file 1) file (+ file 1)) 
				  y <- (list (+ rank 1)) * (> x 0) 
				  * (> y 0) * (< x 9) * (< y 9))))]
		   [(eq? turn 'black)
		    (if (not (eq? rank 7))
			(lc (cons x y) : x <- (list (- file 1) file (+ file 1))
			    y <- (list (- rank 1)) * (> x 0) 
			    * (> y 0) * ( < x 9) * (< y 9))     
			(cons (cons file (- rank 2)) 
			      (lc (cons x y) : x <- (list (- file 1) file (+ file 1)) 
				  y <- (list (- rank 1)) * (> x 0)
				  * (> y 0) * ( < x 9) * (< y 9))))]
		   [else (error "Invalid turn")])))))

(define knight%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)])
	     (append (lc (cons x y) : x <- (list (- file 1) (+ file 1)) 
			 y <- (list (- rank 2) (+ rank 2))
			 * (> x 0) * (> y 0) * ( < x 9) * (< y 9))
		     (lc (cons x y) : x <- (list (- file 2) (+ file 2))
			 y <- (list (- rank 1) (+ rank 1))
			 * (> x 0) * (> y 0) * ( < x 9) * (< y 9)))))))

(define bishop%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)])
	     (append (lc (cons (+ file x) (+ rank x)) : x <- move-range 
			 * (> (+ file x) 0) * (> (+ rank x) 0) 
			 * (< (+ file x) 9) * (< (+ rank x) 9) * (not (= x 0)))
		     (lc (cons (+ file x) (- rank x)) : x <- move-range 
			 * (> (+ file x) 0) * (> (- rank x) 0) * (< (+ file x) 9)
			 * (< (- rank x) 9) * (not (= x 0))))))))

(define rook%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)])
	     (append (lc (cons (+ file x) rank) : x <- move-range 
			 * (> (+ file x) 0) * (< (+ file x) 9) * (not (= x 0)))
		     (lc (cons file (+ rank x)) : x <- move-range 
			 * (> (+ rank x) 0) * (< (+ rank x) 9) * (not (= x 0))))))))

(define queen%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)])
	     (append (lc (cons (+ file x) (+ rank x)) : x <- move-range 
			 * (> (+ file x) 0) * (> (+ rank x) 0) * (< (+ file x) 9) 
			 * (< (+ rank x) 9) * (not (= x 0)))
		     (lc (cons (+ file x) (- rank x)) : x <- move-range
			 * (> (+ file x) 0) * (> (- rank x) 0) * (< (+ file x) 9)
			 * (< (- rank x) 9) * (not (= x 0)))
		     (lc (cons (+ file x) rank) : x <- move-range 
			 * (> (+ file x) 0) * (< (+ file x) 9) * (not (= x 0)))
		     (lc (cons file (+ rank x)) : x <- move-range 
			 * (> (+ rank x) 0) * (< (+ rank x) 9) * (not (= x 0))))))))

(define king%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)])
	     (append (lc (cons x y) : x <- (list (- file 1) file (+ file 1))
			 y <- (list (- rank 1) rank (+ rank 1))
			 * (> x 0) * (< x 9) * (> y 0) * (< y 9)
			 * (or (not (= x file)) (not (= y file)))))))))

(define (make-2d-vector r c)
  (build-vector r (lambda (x) (make-vector c #f))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (vector-set! (vector-ref vec r) c val))

(define board%
  (class object%
	 (super-new)
	 (define board-position initial-position)
	 (define move 'white)
	 (define chess-board (make-2d-vector 8 8))
	 (define dummy-pos (cons 0 0))
	 ;; (define (set-up)
	 ;;   (let* ([posn-for-white (car initial-position)]
	 ;; 	  [posn-for-black (cdr initial-position)])
	 ;;     (set-up
	 (define dummy-pawn (make-object pawn% dummy-pos))
	 (define dummy-knight (make-object knight% dummy-pos))
	 (define dummy-rook (make-object rook% dummy-pos))
	 (define dummy-bishop (make-object bishop% dummy-pos))
	 (define dummy-queen (make-object queen% dummy-pos))
	 (define dummy-king (make-object king% dummy-pos))
	 (define (get-dummy symbol pos)
	   (cond [(eq? symbol 'P) (begin
				    (send dummy-pawn set-position! pos)
				    (send dummy-pawn set-move! move)
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
				   (list) (send (get-dummy (car piece-set) pos) give-all-moves)) pos-lst))
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