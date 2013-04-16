#lang racket

(struct node (posn children alpha beta) #:transparent #:mutable)
;Traditionally, posn holds the node value. 
;Here it will hold a board position, the children will be simulated by calling give-all-moves on this position
;Children will no longer be a field of this struct
;And the value of this position will be calculated only when called for (inside the two alpha-beta functions)

;TO ELIMINATE THE MEMBER CHILDREN OF THE NODE STRUCT
(define inf 10000)
(define tree (node 1 
                   (list 
                    (node 2 (list (node 200 '() (- 0 inf) inf) (node 6 '() (- 0 inf) inf)) (- 0 inf) inf) 
                    (node 3 (list (node 66 '() (- 0 inf) inf) (node 7 '() (- 0 inf) inf)) (- 0 inf) inf)
                    (node 4 (list (node 66 '() (- 0 inf) inf) (node 9 '() (- 0 inf) inf)) (- 0 inf) inf)
                    (node 5 (list (node 66 '() (- 0 inf) inf) (node 8 '() (- 0 inf) inf)) (- 0 inf) inf)
                    (node 6 (list (node 66 '() (- 0 inf) inf) (node 10 '() (- 0 inf) inf)) (- 0 inf) inf))
                   (- 0 inf) 
                   inf))

;One workaround to generate the tree on-the-fly is to define a variable (say, "children") inside the alpha-beta functions,
;whose value is the list of all board positions that are children of the current board "posn".
;With this we need to replace all "(node-children curnode)" calls with "children" (except in the set! one), when it just has to be removed!
(define (alpha-beta-max curnode depthleft)
  (define (one-child-check remaining-children)
    (if (null? remaining-children) (node-alpha curnode)
    (let* ([child (car remaining-children)]
           [score (alpha-beta-min child (- depthleft 1))])
      (cond [(>= score (node-beta curnode)) (node-beta curnode)]
            [(> score (node-alpha curnode)) (begin 
                                           (set-node-alpha! curnode score)
                                           ;This set! call is only and only to update alpha. Can it be optimized?
                                           (one-child-check (cdr remaining-children)))]
            [else (one-child-check (cdr remaining-children))]))))
  (if (or (= depthleft 0) (null? (node-children curnode))) (node-posn curnode) (one-child-check (node-children curnode))))
;EVALUATE BOARD POSN HERE!!!



;#################################################
;VERY IMPORTANT- MAKE THE CALLS TO ANY OF THE FUNCTIONS WITH CURNODE= NEWLY CONSTRUCTED NODE WITH THE GIVEN POS, ALPHA AND BETA
;#################################################

(define (alpha-beta-min curnode depthleft)
  (define (one-child-check remaining-children)
    (if (null? remaining-children) (node-beta curnode)
    (let* ([child (car remaining-children)]
           [score (alpha-beta-max child (- depthleft 1))])
      (cond [(<= score (node-alpha curnode)) (node-alpha curnode)]
            [(< score (node-beta curnode)) (begin 
                                           (set-node-beta! curnode score)
                                           (one-child-check (cdr remaining-children)))]
            [else (one-child-check (cdr remaining-children))]))))
  (if (or (= depthleft 0) (null? (node-children curnode))) (node-posn curnode) (one-child-check (node-children curnode))))
;EVALUATE BOARD POSN HERE!!!


;This one is a modified alpha-beta-max function which returns the position instead of the value
(define (first-alpha-beta-max curnode depthleft)
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
    (let* ([child (car remaining-children)]
           [score (alpha-beta-min child (- depthleft 1))])
      (cond [(>= score (node-beta curnode)) (if (and (> randomizer abfailparameter) 
                                                     (not (equal? #f (vector-ref moves-found 0))))
                                                (let* ([chosen-move (choose-move-alpha)])
                                                (if (equal? chosen-move #f) (vector-ref moves-found 0) chosen-move))
                                                ;If choose-move-alpha returns a move with an index that isn't even initialized, 
                                                ;screw everything and choose the best stored move
                                                (node-posn child))]
            ;The above call is made if alpha exceeds beta. Choose the current move with probability abfailparameter or one of the stored moves (calling choose-move again)
            
            [(> score (node-alpha curnode)) (begin 
                                           (set-node-alpha! curnode score)
                                           (move-shift (node-posn child))
                                           ;(display moves-found) (newline) ;Trace output
                                           (one-child-check (cdr remaining-children)))]
            [else (one-child-check (cdr remaining-children))]))))
  (if (or (= depthleft 0) (null? (node-children curnode))) (node-posn curnode) (one-child-check (node-children curnode))))