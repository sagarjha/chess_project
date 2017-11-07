#lang racket
(provide (all-defined-out))

(define (concat l) (foldr append `() l))

(define-syntax lc
  (syntax-rules (: <- *)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : * guard) (if guard (list expr) `())]
    [(lc expr : * guard  qualifier ...) 
     (concat (lc (lc expr : qualifier ...) : * guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (concat (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))
