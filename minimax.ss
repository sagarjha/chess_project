#lang racket
(require "chess.ss")
(require "evaluation.ss")
(require "positions.ss")

(struct node (comp-posn alpha beta) #:transparent #:mutable)


(define inf 10000)

;One workaround to generate the tree on-the-fly is to define a board inside (say, "minboard") inside the alpha-beta functions,
;sending to which give-all-positions will return a list of positions for me.
;With this we need to replace all "(node-children curnode)" calls with a "send give-all-positions to minboard".
(define (alpha-beta-max curnode depthleft)
  (define minboard (new board% (complete-board-position (node-comp-posn curnode))))
  (define (one-child-check remaining-children)
    (if (null? remaining-children) (node-alpha curnode)
    (let* ([childposn (car remaining-children)]
           [child (node childposn (node-alpha curnode) (node-alpha curnode))]
           [score (alpha-beta-min child (- depthleft 1))])
      (cond [(>= score (node-beta curnode)) (node-beta curnode)]
            [(> score (node-alpha curnode)) (begin 
                                           (set-node-alpha! curnode score)
                                           (one-child-check (cdr remaining-children)))]
            [else (one-child-check (cdr remaining-children))]))))
  (let* ([list-of-child-posns (send minboard give-all-positions)]) 
    (if (or (= depthleft 0) (null? list-of-child-posns)) 
        (evaluate-posn (node-comp-posn curnode)) 
        (one-child-check list-of-child-posns))))

(define (alpha-beta-min curnode depthleft)
  (define minboard (new board% (complete-board-position (node-comp-posn curnode))))
  (define (one-child-check remaining-children)
    (if (null? remaining-children) (node-beta curnode)
    (let* ([childposn (car remaining-children)]
           [child (node childposn (node-alpha curnode) (node-alpha curnode))]
           [score (alpha-beta-max child (- depthleft 1))])
      (cond [(<= score (node-alpha curnode)) (node-alpha curnode)]
            [(< score (node-beta curnode)) (begin 
                                           (set-node-beta! curnode score)
                                           (one-child-check (cdr remaining-children)))]
            [else (one-child-check (cdr remaining-children))]))))
(let* ([list-of-child-posns (send minboard give-all-positions)]) 
    (if (or (= depthleft 0) (null? list-of-child-posns)) 
        (evaluate-posn (node-comp-posn curnode)) 
        (one-child-check list-of-child-posns))))


;This one is a modified alpha-beta-max function which returns the position instead of the value
(define (first-alpha-beta-max curnode depthleft)
  (define minboard (new board% (complete-board-position (node-comp-posn curnode))))
  (define randomizer (random));Random number to facilitate selection of second-best move with some probability
  (define abfailparameter 0.97);This is to choose probabilistically (in case of alpha > beta) which move to take- the current one or one from the already found ones
  (define moves-found (make-vector 5 #f))
  (define (choose-move)
    (cond [(< randomizer 0.85) (vector-ref moves-found 0)]
          [(< randomizer 0.90) (vector-ref moves-found 1)]
          [(< randomizer 0.94) (vector-ref moves-found 2)]
          [(< randomizer 0.97) (vector-ref moves-found 3)]
          [else (vector-ref moves-found 4)]))
  (define (choose-move-alpha)
    (let* ([randomizer2 (random)])
      (cond [(< randomizer2 0.85) (vector-ref moves-found 0)]
            [(< randomizer2 0.90) (vector-ref moves-found 1)]
            [(< randomizer2 0.94) (vector-ref moves-found 2)]
            [(< randomizer2 0.97) (vector-ref moves-found 3)]
          [else (vector-ref moves-found 4)])))
  (define (move-shift childposn) (begin (vector-set! moves-found 4 (vector-ref moves-found 3))
                                        (vector-set! moves-found 3 (vector-ref moves-found 2))
                                        (vector-set! moves-found 2 (vector-ref moves-found 1))
                                        (vector-set! moves-found 1 (vector-ref moves-found 0))
                                        (vector-set! moves-found 0 childposn)))
  (define (one-child-check remaining-children)
    (if (null? remaining-children) 
        (if (equal? (choose-move) #f) (vector-ref moves-found 0) (choose-move))
    (let* ([childposn (car remaining-children)]
           [child (node childposn (node-alpha curnode) (node-alpha curnode))]
           [score (alpha-beta-min child (- depthleft 1))])
      (cond [(>= score (node-beta curnode)) (if (and (> randomizer abfailparameter) 
                                                     (not (equal? #f (vector-ref moves-found 0))))
                                                (let* ([chosen-move (choose-move-alpha)])
                                                (if (equal? chosen-move #f) (vector-ref moves-found 0) chosen-move))
                                                ;If choose-move-alpha returns a move with an index that isn't even initialized, 
                                                ;screw everything and choose the best stored move
                                                (node-comp-posn child))]
            ;The above call is made if alpha exceeds beta. Choose the current move with probability abfailparameter or one of the stored moves (calling choose-move again)
            
            [(> score (node-alpha curnode)) (begin 
                                           (set-node-alpha! curnode score)
                                           (move-shift (node-comp-posn child))
                                           ;(display (node-comp-posn child)) (newline)
                                           ;(display moves-found) (newline) ;Trace output
                                           (one-child-check (cdr remaining-children)))]
            [else (one-child-check (cdr remaining-children))]))))
  (let* ([list-of-child-posns (send minboard give-all-positions)]) 
    (if (or (= depthleft 0) (null? list-of-child-posns)) 
        (evaluate-posn (node-comp-posn curnode)) 
        (one-child-check list-of-child-posns))))

;(define B (new board% (board-position random-position) (move 'white)))
;(send B give-all-positions)

(define random-pos (cons (list
                          (list "P" (cons 1 2) (cons 2 2) (cons 3 2) (cons 4 2) (cons 5 4) (cons 6 2) (cons 7 2) (cons 8 2))
                          (list "R" (cons 1 1) (cons 6 1))
                          (list "N" (cons 2 1) (cons 6 3))
                          (list "B" (cons 3 1) (cons 2 5))
                          (list "Q" (cons 4 1))
                          (list "K" (cons 7 1)))
                         (list 
                          (list "P" (cons 1 7) (cons 2 7) (cons 3 7) (cons 4 7) (cons 5 5) (cons 6 7) (cons 7 7) (cons 8 7))
                          (list "R" (cons 1 8) (cons 8 8))
                          (list "N" (cons 3 6) (cons 6 6))
                          (list "B" (cons 3 8) (cons 6 8))
                          (list "Q" (cons 4 8))
                          (list "K" (cons 5 8)))))

(define comp-posn-trial (new complete-position% (board-position random-pos) (move (cons 0 0)) (turn 'white)
                             (lceb #t) (sceb #t) (lcew #t) (scew #t)))

;(define tminboard (new board% (board-position random-pos) (move 'white)))


(define (get-best-move board-comp-pos) (first-alpha-beta-max (node board-comp-pos (- 0 inf) inf) 1));Last argument is tree depth. To change

(define returned-move (get-best-move initial-complete-position))