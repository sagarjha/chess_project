#lang racket
(require "2D-vector.ss")
(require "positions.ss")
(require "my-canvas.ss")
(require "lc.ss")
(require "chess-simulator.ss")
(require racket/gui)
(require racket/draw)
(provide (all-defined-out))
		
(define chess-gui%
  (class object%
    (init-field chance)
    (init-field (load-complete-position initial-complete-position))
    (define load-position (get-field board-position load-complete-position))
    (define prom-pref #f)
    (define curr-posn '())
    (define chess-game (new chess% (complete-position load-complete-position)))
    (define chess_frame (new frame%
                             [label "Chess"]
                             [width 990]
                             [height 740]))
    
    (define mypanel (new horizontal-panel%
                         [parent chess_frame]))
    (define click 0)
    (define board (read-bitmap "pieces2/board.jpg"))
    (define mdc (new bitmap-dc% [bitmap board]))
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
    
   
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
    
   (define (user-sel c e)
     (display "user-sel")
     (define (f i)
             (define (g j)
                     (if (> j 8)
                         (void)
                         (begin
                           (2d-vector-set! vec-bit (- i 1) (- j 1) #f) 
                           (draw-blank i j)
                           (g (+ j 1)))))
             (if (> i 8)
                 (void)
                 (begin
                   (g 1)
                   (f (+ i 1)))))
     (if (equal? (send e get-event-type) 'list-box-dclick)
         (begin
           (f 1)
           (let* ([thelist (send chess-game get-object-position-list)]
                  [myposn (list-ref  thelist (- (- (length thelist) (car (send c get-selections))) 1))])
             (begin 
               (if (equal? myposn curr-posn)
                   (set-field! handler cb (lambda (event) 
                                            (handle-left-down event)))
                   (set-field! handler cb (void)))
               (show-gui myposn)))
             (show))
           (void)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (choice-sel c e)
      (if (equal? (send e get-event-type) 'list-box-dclick)
          (begin
            (display "in-select")
            (newline)
            (if (= (send c get-selection) 0)
                (set! prom-pref "Q")
                (set! prom-pref "N")))
            (void)))
   
    
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define scroll-list (new list-box%
                             [label "Look Back"]	 
                             [choices '()]	 
                             [parent mypanel]
                             [style (list 'single 'vertical-label)]
                             [min-width 150]
                             [stretchable-width #f]
                             [callback user-sel]))
    
     (define mychoice (new list-box%	 
                          [label "Promotion Choice"]	 
                          [choices (list "Queen" "Knight")]
                          [style (list 'single 'vertical-label)]
                          [parent mypanel]
                          [stretchable-width #f]
                          [stretchable-height #f]
                          [callback choice-sel]))
    
    (send mychoice enable #f)
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    
    (define (draw-blank x y)
      (if (odd? (+ x y))
            (send mdc draw-bitmap (read-bitmap "pieces2/green_box.jpg") (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
            (send mdc draw-bitmap (read-bitmap "pieces2/white_box.jpg") (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (define (ex-cas x1 y1 x2 y2 type)
      (begin
        (draw-blank x1 y1)
        (draw-blank x2 y2)
        (let* ([bit-king (2d-vector-ref vec-bit (- x1 1) (- y1 1))]
               [bit-rook (2d-vector-ref vec-bit (- x2 1) (- y2 1))])
          (begin
            (display bit-king)
            (newline)
            (2d-vector-set! vec-bit (- x1 1) (- y1 1) #f)
            (2d-vector-set! vec-bit (- x2 1) (- y2 1) #f)
            (if (= type 1)
                (begin
                  (draw-blank (+ x1 2) y1)
                  (draw-blank (+ x1 1) y1)
                  (send mdc draw-bitmap (car bit-king) (+ 50 (* (- (+ x1 2) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! vec-bit (- (+ x1 2) 1) (- y1 1) bit-king)
                  (send mdc draw-bitmap (car bit-rook) (+ 50 (* (- (+ x1 1) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! vec-bit (- (+ x1 1) 1) (- y1 1) bit-rook))
                (begin
                  (draw-blank (- x1 2) y1)
                  (draw-blank (- x1 1) y1)
                  (send mdc draw-bitmap (car bit-king) (+ 50 (* (- (- x1 2) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! vec-bit (- (- x1 2) 1) (- y1 1) bit-king)
                  (send mdc draw-bitmap (car bit-rook) (+ 50 (* (- (- x1 1) 1) 80)) (+ 50 (* (- 9 (+ y1 1)) 80)))
                  (2d-vector-set! vec-bit (- (- x1 1) 1) (- y1 1) bit-rook))
                )))))
                
                
                
          
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
      
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
                (draw-blank xp yp)
                (if (not (equal? (2d-vector-ref vec-bit (- x 1) (- y 1)) #f))
                    (draw-blank x y)
                    (void))
                (send mdc draw-bitmap (car bit-mov) (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
                (2d-vector-set! vec-bit (- xp 1) (- yp 1) #f)
                (2d-vector-set! vec-bit (- x 1) (- y 1) bit-mov)
                (send scroll-list append (string-append colour ", " item ", (" (number->string xp) "," (number->string yp) ") (" (number->string x) "," (number->string y) ")" ))))
           
              (let* ([colour (first move)]
                   [castl (second move)])
                (begin
                  (cond ((and (equal? colour "white") (equal? castl "O-O")) (ex-cas 5 1 8 1 1)) 
                        ((and (equal? colour "white") (equal? castl "O-O-O")) (ex-cas 5 1 1 1 2))
                        ((and (equal? colour "black") (equal? castl "O-O")) (ex-cas 5 8 8 8 1)) 
                        ((and (equal? colour "black") (equal? castl "O-O-O")) (ex-cas 5 8 1 8 2)))
                  (send scroll-list append (string-append colour "," castl))))) 
      (show)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (show)
      (send cb refresh)
      (sleep/yield 0.1))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    (define/public (findbox x y)
      (if (or (< x 50) (> x 690) (< y 50) (> y 690))
          (void)
          (if (and(and (> (remainder (- x 50) 80) 10) (< (remainder (- x 50) 80) 70))
                  (and (> (remainder (- y 50) 80) 10) (< (remainder (- y 50) 80) 70)))
              (cons (+ (quotient (- x 50) 80) 1) (- 8 (quotient (- y 50) 80)))
              (void))))	
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    (define/public (show-gui initial-position)
      (define (show-gui-h piece posn colour)
        (if (null? posn)
            (void)
            (begin
              (let* ([curr-bitmap (read-bitmap (string-append "pieces2/" colour piece ".png"))])
                (begin
                  (send mdc draw-bitmap curr-bitmap (+ 50 (* (- (caar posn) 1) 80)) (+ 50 (* (- 9 (+ (cdar posn) 1)) 80)))
                  (2d-vector-set! vec-bit (- (caar posn) 1) (- (cdar posn) 1) (cons curr-bitmap piece))))
              (show-gui-h piece (cdr posn) colour))))
      (define (set colour list)
        (if (null? list)
            (void)
            (begin
              (show-gui-h (caar list) (cdar list) colour)
              (set colour (cdr list)))))
      (begin
        (set "w" (car initial-position))
        (set "b" (cdr initial-position))
        (sleep/yield 0.1)
        (send chess_frame show #t)
        ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (show-gui load-position)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (driver)
      (send chess-game computer-play)
      (set! curr-posn (car (send chess-game get-object-position-list)))
      (update-posn (cons (send chess-game get-turn) (send chess-game get-move-list))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (paint dc)
      (send dc draw-bitmap board 0 0))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (if-special hum-move)
      (let* ([sec (second hum-move)]
             [third (third hum-move)])
        (if (equal? (first hum-move) "K")
            (cond [(and (equal? sec (cons 5 1))
                        (equal? third (cons 7 1)))
                   (begin
                     (draw-blank 8 1)
                     (send mdc draw-bitmap (read-bitmap (string-append "pieces2/wR.png")) (+ 50 (* (- 6 1) 80)) (+ 50 (* (- 9 (+ 1 1)) 80)))
                     (send scroll-list
                           append 
                           (string-append 
                            "white" ", " "O-O"))
                     #t)]
                  [(and (equal? sec (cons 5 8))
                        (equal? third (cons 7 8)))
                   (begin
                     (draw-blank 8 8)
                     (send mdc draw-bitmap (read-bitmap (string-append "pieces2/bR.png")) (+ 50 (* (- 6 1) 80)) (+ 50 (* (- 9 (+ 8 1)) 80)))
                     (send scroll-list
                           append 
                           (string-append 
                            "black" ", " "O-O"))
                     #t)]
                  [(and (equal? sec (cons 5 1))
                        (equal? third (cons 3 1)))
                   (begin
                     (draw-blank 1 1)
                     (send mdc draw-bitmap (read-bitmap (string-append "pieces2/wR.png")) (+ 50 (* (- 4 1) 80)) (+ 50 (* (- 9 (+ 1 1)) 80)))
                      (send scroll-list
                           append 
                           (string-append 
                            "white" ", " "O-O-O"))
                     #t)]
                  [(and (equal? sec (cons 5 8))
                        (equal? third (cons 3 8)))
                   (begin
                     (draw-blank 1 8)
                     (send mdc draw-bitmap (read-bitmap (string-append "pieces2/bR.png")) (+ 50 (* (- 4 1) 80)) (+ 50 (* (- 9 (+ 8 1)) 80)))
                      (send scroll-list
                           append 
                           (string-append 
                            "black" ", " "O-O-O"))
                     #t)])
            #f)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (define (if-prom xp yp x y)
     (begin
      (cond [(= y 8)
             (begin
               (send mychoice enable #t)
               (if (equal? prom-pref #f)
                   (begin (display "I m false") (newline)(set! prom-pref "Q"))
                   (void))
               (draw-blank x y)
               (send mdc draw-bitmap (read-bitmap (string-append "pieces2/w" prom-pref ".png")) (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
               (2d-vector-set! vec-bit (- x 1) (- y 1) (cons (read-bitmap (string-append "pieces2/w" prom-pref ".png")) prom-pref)))]
            [(= y 1)
             (begin
               (send mychoice enable #t)
               (if (equal? prom-pref #f)
                   (set! prom-pref "Q")
                   (void))
               (draw-blank x y)
               (send mdc draw-bitmap (read-bitmap (string-append "pieces2/b" prom-pref ".png")) (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
               (2d-vector-set! vec-bit (- x 1) (- y 1) (cons (read-bitmap (string-append "pieces2/b" prom-pref ".png")) prom-pref)))]
            [else (void)])
      (send mychoice enable #f)
      (set! prom-pref #f)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
               
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (handle-left-down event)
      (display "left-down")
      (send mdc set-pen "red" 2 'solid)
      (send mdc set-brush "red" 'transparent)
      (let* ([box (findbox (send event get-x) (send event get-y))])
        (if (void? box)
            (if (equal? click 1)
                (begin
                  (display (cdr prev))
                  (let* ([xp (car prev)]
                         [yp (cdr prev)])
                    (draw-blank xp yp)
                    (send mdc draw-bitmap (car (2d-vector-ref vec-bit (- xp 1) (- yp 1))) (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80))))
                  (set! prev (cons #f #f))
                  (set! click 0)
                  (show))
                (void))
  
            (let* ([tx (+ (+ (* (- (car box) 1) 80) 50) 2)]
                   [ty (+ (+ (* (- 8 (cdr box)) 80) 50) 2)])
              
              (cond [(and (equal? (2d-vector-ref vec-bit (- (car box) 1) (- (cdr box) 1)) #f) (equal? click 0)) (void)]
                    [(equal? click 1)
                     (begin
                       (let* ([x (car box)]
                              [y (cdr box)]
                              [xp (car prev)]
                              [yp (cdr prev)]
                              [bit-mov (2d-vector-ref vec-bit (- xp 1) (- yp 1))]
                              [bit-pres (2d-vector-ref vec-bit (- x 1) (- y 1))]
                              [tx (+ (+ (* (- x 1) 80) 50) 2)]
                              [ty (+ (+ (* (- 8 y) 80) 50) 2)]
                              [hum-move (list (cdr bit-mov) prev box)])
                         (begin
                           (display "inside kuchh bhi")
                           (newline)
                           (send mdc draw-rectangle tx ty 78 78)
                           (show)
                           (if (send chess-game human-play hum-move)
                               (begin
                                 (set! curr-posn (car (send chess-game get-object-position-list)))
                                 (draw-blank xp yp)
                                 (draw-blank x y)
                                 
                                 (send mdc draw-bitmap (car bit-mov) (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
                                 (2d-vector-set! vec-bit (- xp 1) (- yp 1) #f)
                                 (2d-vector-set! vec-bit (- x 1) (- y 1) bit-mov)
                                 (display "CORRRRRR")
                                 (newline)
                                 
;                                 (display "vec-bit")
;                                 (newline)
;                                 (display (lc (2d-vector-ref vec-bit x y): x <- (list 0 1 2 3 4 5 6 7) y <- (list 0 1 2 3 4 5 6 7)))
;                                 (newline)
                                 (if (equal? (cdr bit-mov) "P")
                                     (if-prom xp yp x y)
                                     (void))
                                 (if (equal? (if-special hum-move) #f)
                                     (send scroll-list
                                           append 
                                           (string-append 
                                            chance ", " 
                                            (cdr bit-mov)
                                            ", (" 
                                            (number->string xp) "," (number->string yp) ") (" (number->string x) "," (number->string y) ")" ))
                                     #f)
                                 (show)
                                 (driver))
                               
                               (begin
                                 (display "INCORRRR")
                                 (draw-blank xp yp)
                                 (draw-blank x y)
                                 (send mdc draw-bitmap (car bit-mov) (+ 50 (* (- xp 1) 80)) (+ 50 (* (- 9 (+ yp 1)) 80)))
                                 (if (not (equal? bit-pres #f))
                                     (send mdc draw-bitmap (car bit-pres) (+ 50 (* (- x 1) 80)) (+ 50 (* (- 9 (+ y 1)) 80)))
                                     (void))
                                 (show)))
                           (set! click 0))))]
                    
                    [(and (not (equal? (2d-vector-ref vec-bit (- (car box) 1) (- (cdr box) 1)) #f)) (equal? click 0)) 
                     (begin
                       (if (or (and (equal? (cdr box) 7) (equal? chance "white"))
                               (and (equal? (cdr box) 2) (equal? chance "black")))
                           (send mychoice enable #t)
                           (void))
                       (set! click 1)
                       (send mdc draw-rectangle tx ty 78 78)
                       (set! prev (cons (car box) (cdr box)))
                       (show))])))))
    
    (if (equal? chance "black")
        (driver)
        (void))
    
    (super-new)))

;(define mygui (new chess-gui%
;                   [chance "white"]))




