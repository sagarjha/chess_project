#lang racket

(provide make-2d-vector
	 2d-vector-ref
	 2d-vector-set!)

(define (make-2d-vector r c)
  (build-vector r (lambda (x) (make-vector c #f))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (vector-set! (vector-ref vec r) c val))
