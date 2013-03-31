#lang racket

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
	 (define/public (move-ahead)
	   (let ([pos (send this get-position)])
	     (if (eq? (cdr pos) 7)
		 (error "Should be promoted")
		 (send this set-position! (cons (car pos) (+ 1 (cdr pos)))))))
	 (define/public (capture dir)
	   (let ([pos (send this get-position)])
	     (cond [(and (eq? dir 0) (eq? (car pos) 1)) (error "Illegal move")]
		   [(eq? dir 0) (send this set-position 
				      (cons (- (car pos) 1) (+ 1 (cdr pos))))])))))