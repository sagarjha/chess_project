#lang racket

(provide (all-defined-out))

(define (take-while pred1 pred2 lst)
  (foldr (lambda (val l)
	   (cond [(and (pred1 val) (pred2 val)) (cons val l)]
		 [(and (pred1 val) (not (pred2 val)) (list val))]
		 [else (list)]))
	 (list) lst))

(define (remove-val val lst)
  (foldr (lambda (elem l)
	   (if (eq? val elem)
	       l
	       (cons elem l)))
	 (list) lst))

