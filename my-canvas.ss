#lang racket
(require racket/gui)
(require racket/draw)
(provide (all-defined-out))

(define my-canvas%
  (class canvas% 
    (init-field handler)
    (define/override (on-event event)
      (cond [(equal? (send event get-event-type) 'left-down) (handler event)]
            [else null]))
    (super-new)))