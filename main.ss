#lang racket
(require "2D-vector.ss")
(require "initialPosition.ss")
(require "randomPosition.ss")
(require "chess_gui.ss")
(require racket/gui)
(require racket/draw)

(define main-window%
  (class object%
    (define main_frame (new frame%
                             [label "Main"]
                             [width 740]
                             [height 740]))
    (define main-win (read-bitmap "pieces2/chess.jpg"))
    (define cb (new my-canvas% 
                    [parent main_frame]
                    [handler 
                     (lambda (event) 
                       (handle-left-down event))]
                    [paint-callback
                     (lambda (canvas dc) 
                       (paint dc))]))
    (define (paint dc)
      (send dc draw-bitmap main-win 0 0))
    
    (define/public (show)
      (send main_frame show #t))
    
    (define (handle-left-down event)
      (let* ([x (send event get-x)]
             [y (send event get-y)])
        (cond ((and (> x 50) (< x 150) (> y 150) (< y 200))
               (let* ([mychessboard (new chess-gui%)])
                 (begin
                   (send mychessboard show)
                   (send main_frame show #f)))
               (void))
              ((and (> x 50) (< x 150) (> y 325) (< y 375))
               (send main_frame show #f))
              (else (void)))))
    (super-new)))
            
(define mywindow (new main-window%))
(send mywindow show)