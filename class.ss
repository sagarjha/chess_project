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
	 (super-new)
	 (define/public (give-all-moves turn)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)])
	     (cond [(eq? turn 'white)
		    (if (not (eq? rank 2))
			(lc (cons x y) : x <- (list (- file 1) file (+ file 1))
			    y <- (list (+ rank 1)) * (> x 0) * (> y 0) * ( < x 9) * (< y 9))     
			(cons (cons file (+ rank 2)) 
			      (lc (cons x y) : x <- (list (- file 1) file (+ file 1)) y <- (list (+ rank 1)) * (> x 0) * (> y 0) * (< x 9) * (< y 9))))]
		   [(eq? turn 'black)
		    (if (not (eq? rank 7))
			(lc (cons x y) : x <- (list (- file 1) file (+ file 1))
			    y <- (list (- rank 1)) * (> x 0) * (> y 0) * ( < x 9) * (< y 9))     
			(cons (cons file (- rank 2)) 
			      (lc (cons x y) : x <- (list (- file 1) file (+ file 1)) y <- (list (- rank 1)) * (> x 0) * (> y 0) * ( < x 9) * (< y 9))))]
		   [else (error "Invalid turn")])))))

(define knight%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)])
	     (append (lc (cons x y) : x <- (list (- file 1) (+ file 1)) y <- (list (- rank 2) (+ rank 2)) * (> x 0) * (> y 0) * ( < x 9) * (< y 9))
		     (lc (cons x y) : x <- (list (- file 2) (+ file 2)) y <- (list (- rank 1) (+ rank 1)) * (> x 0) * (> y 0) * ( < x 9) * (< y 9)))))))

(define bishop%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)])
	     (append (lc (cons (+ file x) (+ rank x)) : x <- move-range * (> (+ file x) 0) * (> (+ rank x) 0) * (< (+ file x) 9) * (< (+ rank x) 9) * (not (= x 0)))
		     (lc (cons (+ file x) (- rank x)) : x <- move-range * (> (+ file x) 0) * (> (- rank x) 0) * (< (+ file x) 9) * (< (- rank x) 9) * (not (= x 0))))))))

(define rook%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)])
	     (append (lc (cons (+ file x) rank) : x <- move-range * (> (+ file x) 0) * (< (+ file x) 9) * (not (= x 0)))
		     (lc (cons file (+ rank x)) : x <- move-range * (> (+ rank x) 0) * (< (+ rank x) 9) * (not (= x 0))))))))

(define queen%
  (class chess-piece%
	 (inherit-field pos)
	 (super-new)
	 (define/public (give-all-moves)
	   (let* ([pos (send this get-position)]
		  [file (car pos)]
		  [rank (cdr pos)]
		  [move-range (range -7 8)])
	     (append (lc (cons (+ file x) (+ rank x)) : x <- move-range * (> (+ file x) 0) * (> (+ rank x) 0) * (< (+ file x) 9) * (< (+ rank x) 9) * (not (= x 0)))
		     (lc (cons (+ file x) (- rank x)) : x <- move-range * (> (+ file x) 0) * (> (- rank x) 0) * (< (+ file x) 9) * (< (- rank x) 9) * (not (= x 0)))
		     (lc (cons (+ file x) rank) : x <- move-range * (> (+ file x) 0) * (< (+ file x) 9) * (not (= x 0)))
		     (lc (cons file (+ rank x)) : x <- move-range * (> (+ rank x) 0) * (< (+ rank x) 9) * (not (= x 0))))))))

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
	 (define chess-board (make-2d-vector 8 8))
	 ;; (define (set-up)
	 ;;   (let* ([posn-for-white (car initial-position)]
	 ;; 	  [posn-for-black (cdr initial-position)])
	 ;;     (set-up
	 (define/public (print)
	   (display chess-board)
	   (newline))))