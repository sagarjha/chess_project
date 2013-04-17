#lang racket
(require "2D-vector.ss")
(require "initialPosition.ss")
(require "randomPosition.ss")
(require racket/gui)
(require racket/draw)
(provide my-canvas%
         chess-gui%)
;(define (make-2d-vector r c)
;  (build-vector r (lambda (x) (make-vector c #f))))
;
;(define (2d-vector-ref vec r c)
;  (vector-ref (vector-ref vec r) c))
;
;(define (2d-vector-set! vec r c val)
;  (vector-set! (vector-ref vec r) c val))

(define my-canvas%
  (class canvas% 
    (init-field handler)
    (define/override (on-event event)
      (cond [(equal? (send event get-event-type) 'left-down) (handler event)]
            [else null]))
    (super-new)))
			
(define chess-gui%
  (class object%
    (define chess_frame (new frame%
                             [label "Chess"]
                             [width 740]
                             [height 740]))
    (define click 0)
    (define prev (cons #f #f))
    (define vec-bit (make-2d-vector 8 8))
    (define cb (new my-canvas% 
                    [parent chess_frame]
                    [handler 
                     (lambda (event) 
                       (handle-left-down event))]
                    [paint-callback
                     (lambda (canvas dc) 
                       (paint dc))]))
    
    (define board (read-bitmap "pieces2/board.jpg"))
    
    (define mdc (new bitmap-dc% [bitmap board]))
    
    
    (define/public (show)
      (send chess_frame show #t))
    
    (define/public (findbox x y)
      (if (or (< x 50) (> x 690) (< y 50) (> y 690))
          (void)
          (if (and(and (> (remainder (- x 50) 80) 10) (< (remainder (- x 50) 80) 70))
                  (and (> (remainder (- y 50) 80) 10) (< (remainder (- y 50) 80) 70)))
              (cons (+ (quotient (- x 50) 80) 1) (- 8 (quotient (- y 50) 80)))
              (void))))	
    
    (define/public (show-gui initial-position)
      (define (show-gui-h piece posn colour)
        (if (null? posn)
            (void)
            (begin
              (let* ([curr-bitmap (read-bitmap (string-append "pieces2/" colour piece ".png"))])
                (begin
                  (send mdc draw-bitmap curr-bitmap (+ 50 (* (- (caar posn) 1) 80)) (+ 50 (* (- 9 (+ (cdar posn) 1)) 80)))
                  (2d-vector-set! vec-bit (- (caar posn) 1) (- (cdar posn) 1) curr-bitmap)))
              (show-gui-h piece (cdr posn) colour))))
      (define (set colour list)
        (if (null? list)
            (void)
            (begin
              (show-gui-h (caar list) (cdar list) colour)
              (set colour (cdr list)))))
      (begin
        (set "w" (car initial-position))
        (set "b" (cdr initial-position))))
    
    (show-gui initial-position)
    (define (paint dc)
      (send dc draw-bitmap board 0 0))
    
    (define (handle-left-down event)
      (display "left-down")
      (send mdc set-pen "red" 2 'solid)
      (send mdc set-brush "red" 'transparent)
      (let* ([box (findbox (send event get-x) (send event get-y))])
        (if (void? box)
            (if (equal? click 1)
                (begin
                  (display "inside")
                  (display (car prev))
                  (newline)
                  (display (cdr prev))
                  (let* ([xp (car prev)]
                         [yp (cdr prev)])
                    (if (odd? (+ xp yp))
                        (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80)))
                        (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80))))
                    (send mdc draw-bitmap (2d-vector-ref vec-bit (- xp 1) (- yp 1)) (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80))))
                  (set! prev (cons #f #f))
                  (set! click 0)
                  (show))
                (void))
              
            (begin
              (display "ok")
              (let* ([tx (+ (+ (* (- (car box) 1) 80) 50) 2)]
                     [ty (+ (+ (* (- 8 (cdr box)) 80) 50) 2)])
               
               (cond [(and (equal? (2d-vector-ref vec-bit (- (car box) 1) (- (cdr box) 1)) #f) (equal? click 0)) (void)]
                     [(equal? click 1)
                      (begin
                        (let* ([x (car box)]
                               [y (cdr box)]
                               [xp (car prev)]
                               [yp (cdr prev)]
                               [bit-mov (2d-vector-ref vec-bit (- xp 1) (- yp 1))])
                          (begin
                            (if (odd? (+ xp yp))
                                (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80)))
                                (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80))))
                            (if (not (equal? (2d-vector-ref vec-bit (- (car box) 1) (- (cdr box) 1)) #f))
                                (if (odd? (+ x y))
                                    (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
                                    (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80))))
                                (void))
                            (send mdc draw-bitmap bit-mov (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
                            (2d-vector-set! vec-bit (- xp 1) (- yp 1) #f)
                            (2d-vector-set! vec-bit (- x 1) (- y 1) bit-mov)))
                            (set! click 0))]
                              
                     [(and (not (equal? (2d-vector-ref vec-bit (- (car box) 1) (- (cdr box) 1)) #f)) (equal? click 0)) 
                      (begin
                        (set! click 1)
                        (display "red")
                        (send mdc draw-rectangle tx ty 78 78)
                        (set! prev (cons (car box) (cdr box))))]))
              (show)))))
    
    (super-new)))


;(define mychessboard (new chess-gui%))
;(send mychessboard show)




