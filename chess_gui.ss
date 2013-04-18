#lang racket
(require "2D-vector.ss")
(require "initialPosition.ss")
(require "randomPosition.ss")
(require "my-canvas.ss")
(require racket/gui)
(require racket/draw)
(provide (all-defined-out))

;(define (make-2d-vector r c)
;  (build-vector r (lambda (x) (make-vector c #f))))
;
;(define (2d-vector-ref vec r c)
;  (vector-ref (vector-ref vec r) c))
;
;(define (2d-vector-set! vec r c val)
;  (vector-set! (vector-ref vec r) c val))


			
(define chess-gui%
  (class object%
    (define chess_frame (new frame%
                             [label "Chess"]
                             [width 890]
                             [height 740]))
    (define mypanel (new horizontal-panel%
                         [parent chess_frame]))
   (define (user-sel c e)
       (if (equal? (send e get-event-type) 'list-box-dclick)
           (begin
             (display (send scroll-list get-data (car (send c get-selections))))
             (newline)
             (display (send scroll-list get-column-labels)))
           (void)))
    
    (define click 0)
    (define prev (cons #f #f))
    (define vec-bit (make-2d-vector 8 8))
    (define cb (new my-canvas% 
                    [parent mypanel]
                    [handler 
                     (lambda (event) 
                       (handle-left-down event))]
                    [paint-callback
                     (lambda (canvas dc) 
                       (paint dc))]))
    
    (define board (read-bitmap "pieces2/board.jpg"))
    
    (define mdc (new bitmap-dc% [bitmap board]))
    
    (define (ex-cas x1 y1 x2 y2 type)
      (begin
        (if (odd? (+ x1 y1))
            (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- x1 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
            (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- x1 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80))))
        (if (odd? (+ x2 y2))
            (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- x2 1) 80)) (+ 50 (* (- 9 (+ y2 1)) 80)))
            (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- x2 1) 80)) (+ 50 (* (- 9 (+ y2 1)) 80))))
        (let* ([bit-king (2d-vector-ref vec-bit (- x1 1) (- y1 1))]
               [bit-rook (2d-vector-ref vec-bit (- x2 1) (- y2 1))])
          (begin
            (2d-vector-set! (- x1 1) (- y1 1) #f)
            (2d-vector-set! (- x2 1) (- y2 1) #f)
            (if (= type 1)
                (begin
                  (send mdc draw-bitmap bit-king (+ 50 (* (- (+ x1 2) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! (- (+ x1 2) 1) (- y1 1) bit-king)
                  (send mdc draw-bitmap bit-rook (+ 50 (* (- (+ x1 3) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! (- (+ x1 3) 1) (- y1 1) bit-rook))
                (begin
                  (send mdc draw-bitmap bit-king (+ 50 (* (- (- x1 2) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! (- (- x1 2) 1) (- y1 1) bit-king)
                  (send mdc draw-bitmap bit-rook (+ 50 (* (- (- x1 1) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! (- (- x1 1) 1) (- y1 1) bit-rook)))))))
                
                
                
          
      
      
      
    (define/public (update-posn move)
      (begin
        (if (not (= (length move) 2))
            (let* ([colour (first move)]
                   [item (second move)]
                   [x (car (fourth move))]
                   [y (cdr (fourth move))]
                   [xp (car (third move))]
                   [yp (cdr (third move))]
                   [bit-mov (2d-vector-ref vec-bit (- xp 1) (- yp 1))])
              (begin
                (if (odd? (+ xp yp))
                    (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80)))
                    (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80))))
                (if (not (equal? (2d-vector-ref vec-bit (- x 1) (- y 1)) #f))
                    (if (odd? (+ x y))
                        (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
                        (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80))))
                    (void))
                (send mdc draw-bitmap bit-mov (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
                (2d-vector-set! vec-bit (- xp 1) (- yp 1) #f)
                (2d-vector-set! vec-bit (- x 1) (- y 1) bit-mov)
                (send scroll-list append (string-append colour ", " item ", (" (number->string xp) "," (number->string yp) ") (" (number->string x) (number->string y) ")" ))))
            (let* ([colour (first move)]
                   [castl (second move)])
                   (cond ((and (equal? colour "white") (equal? "o-o")) (ex-cas 1 5 1 8 1)) 
                         ((and (equal? colour "white") (equal? "o-o-o")) (ex-cas 1 5 1 1 2))
                         ((and (equal? colour "black") (equal? "o-o")) (ex-cas 8 5 8 8 1)) 
                         ((and (equal? colour "black") (equal? "o-o-o")) (ex-cas 8 5 8 1 2))))))
      (show))
      
      
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
    (define scroll-list (new list-box%
                             [label "Look Back"]	 
                             [choices '()]	 
                             [parent mypanel]
                             [style (list 'single 'vertical-label)]
                             [min-width 150]
                             [stretchable-width #f]
                             [callback user-sel]))
    (define (paint dc)
      (send dc draw-bitmap board 0 0))
    
    (define (handle-left-down event)
      ;(display "left-down")
      (send mdc set-pen "red" 2 'solid)
      (send mdc set-brush "red" 'transparent)
      (let* ([box (findbox (send event get-x) (send event get-y))])
        (if (void? box)
            (if (equal? click 1)
                (begin
                  ;(display "inside")
                  ;(display (car prev))
                  ;(newline)
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
              ;(display "ok")
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
                            (2d-vector-set! vec-bit (- x 1) (- y 1) bit-mov)
                            (send scroll-list append "P" #f)))
                            ;(send scroll-list set-selection (send scroll-list get-first-visible-item))))
                            (set! click 0))]
                              
                     [(and (not (equal? (2d-vector-ref vec-bit (- (car box) 1) (- (cdr box) 1)) #f)) (equal? click 0)) 
                      (begin
                        (set! click 1)
                        ;(display "red")
                        (send mdc draw-rectangle tx ty 78 78)
                        (set! prev (cons (car box) (cdr box))))]))
              (show)))))
    
    (super-new)))
