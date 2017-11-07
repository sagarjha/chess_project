#lang racket

(require "2D-vector.ss")
(require "lc.ss")
(require "positions.ss")
(require "list-functions.ss")
(require "chess.ss")

(provide (all-defined-out))

;any-arbit-pos must be the complete position

(struct piece (rank file type color) #:transparent)
  
  (define (evaluate-posn comp-posn)

  (define temp-comp-posn comp-posn)

  (define comp-posn-with-white-move
    (begin
      (set-field! turn temp-comp-posn "white")
      temp-comp-posn))
  
  (define comp-posn-with-black-move
    (begin
      (set-field! turn temp-comp-posn "black")
      temp-comp-posn))
      
    
  (define all-moves-possible-by-white
    (begin
      (map (lambda (complete-position)
	     (get-field board-position complete-position))
	   (send (make-object board% comp-posn-with-white-move) give-all-positions))))

  (define all-moves-possible-by-black
    (map (lambda (complete-position)
	   (get-field board-position complete-position))
	 (send (make-object board% comp-posn-with-black-move) give-all-positions)))


  
    (define board-pos (get-field board-position comp-posn))
    (define whose-turn? (get-field turn comp-posn)) 
  (define fboard (make-2d-vector 8 8))
  ; AS IT STANDS, EVERY VALUE RETURNED WILL BE WHITE-BLACK
  (define white-knights (cdr-if-not-null (car-if-not-null (lc x : x <- (car board-pos) * (equal? (car x) "N")))))
  (define black-knights (cdr-if-not-null (car-if-not-null (lc x : x <- (cdr board-pos) * (equal? (car x) "N")))))
  (define white-rooks (cdr-if-not-null (car-if-not-null (lc x : x <- (car board-pos) * (equal? (car x) "R")))))
  (define black-rooks (cdr-if-not-null (car-if-not-null (lc x : x <- (cdr board-pos) * (equal? (car x) "R")))))
  (define white-bishops (cdr-if-not-null (car-if-not-null (lc x : x <- (car board-pos) * (equal? (car x) "B")))))
  (define black-bishops (cdr-if-not-null (car-if-not-null (lc x : x <- (cdr board-pos) * (equal? (car x) "B")))))
  (define white-pawns (cdr-if-not-null (car-if-not-null (lc x : x <- (car board-pos) * (equal? (car x) "P")))))
  (define black-pawns (cdr-if-not-null (car-if-not-null (lc x : x <- (cdr board-pos) * (equal? (car x) "P")))))
  (define white-queens (cdr-if-not-null (car-if-not-null (lc x : x <- (car board-pos) * (equal? (car x) "Q")))))
  (define black-queens (cdr-if-not-null (car-if-not-null (lc x : x <- (cdr board-pos) * (equal? (car x) "Q")))))
  (define white-king (car (cdr-if-not-null (car-if-not-null (lc x : x <- (car board-pos) * (equal? (car x) "K"))))))
  (define black-king (car (cdr-if-not-null (car-if-not-null (lc x : x <- (cdr board-pos) * (equal? (car x) "K"))))))
  ;The king variables are singletons. All the others are lists.
  
  (define (place-pieces list type color)
    (foldr (lambda (cr anscdr)
             (+ anscdr (begin (2d-vector-set! fboard (- (car cr) 1) (- (cdr cr) 1) (piece (car cr) (cdr cr) type color)) 0))) 0 list))
  
  (define (place-all-pieces) 
    (place-pieces white-knights "N" "white") (place-pieces black-knights "N" "black")
    (place-pieces white-rooks "R" "white") (place-pieces black-rooks "R" "black")
    (place-pieces white-bishops "B" "white") (place-pieces black-bishops "B" "black")
    (place-pieces white-queens "Q" "white") (place-pieces black-queens "Q" "black")
    (place-pieces white-pawns "P" "white") (place-pieces black-pawns "P" "black")
    (place-pieces (list white-king) "K" "white") (place-pieces (list black-king) "K" "black"))
  
  (define one-side-material-at-start 4140)
  ;TOTAL MATERIAL WEIGHT OF ALL PIECES (ONE COLOR ONLY) EQUALS 4140
  
  (define white-material-knights (* 330 (length white-knights)))
  (define black-material-knights (* 330 (length black-knights)))
  (define material-knights (- white-material-knights black-material-knights))
  ;MATERIAL VALUE DIFFERENCE (W - B) FOR KNIGHTS
  
  (define white-material-bishops (* 330 (length white-bishops)))
  (define black-material-bishops (* 330 (length black-bishops)))
  (define material-bishops (- white-material-bishops black-material-bishops))
  ;MATERIAL VALUE DIFFERENCE (W - B) FOR BISHOPS
  
  (define white-material-rooks (* 520 (length white-rooks)))
  (define black-material-rooks (* 520 (length black-rooks)))
  (define material-rooks (- white-material-rooks black-material-rooks))
  ;MATERIAL VALUE DIFFERENCE (W - B) FOR ROOKS
  
  (define white-material-queens (* 980 (length white-queens)))
  (define black-material-queens (* 980 (length black-queens)))
  (define material-queens (- white-material-queens black-material-queens))
  ;MATERIAL VALUE DIFFERENCE (W - B) FOR QUEENS
  
  (define white-material-pawns (* 100 (length white-pawns)))
  (define black-material-pawns (* 100 (length black-pawns)))
  (define material-pawns (- white-material-pawns black-material-pawns))  
  ;MATERIAL VALUE DIFFERENCE (W - B) FOR PAWNS
  
  (define net-white-material
    (+ white-material-knights 
       white-material-bishops 
       white-material-rooks
       white-material-queens
       white-material-pawns))
  
  (define net-black-material
    (+ black-material-knights 
       black-material-bishops 
       black-material-rooks
       black-material-queens
       black-material-pawns))
  
  (define net-total-material
    (- net-white-material net-black-material))
  ;(display "Net material ") (display net-total-material) (newline)
  
  (define (manhattan-distance pos1 pos2)
    (+ (abs (- (car pos1) (car pos2))) (abs (- (cdr pos1) (cdr pos2)))))
  
  (define (chebyshev-distance pos1 pos2)
    (max (abs (- (car pos1) (car pos2))) (abs (- (cdr pos1) (cdr pos2)))))
  
  (define (other-type type)
    (if (equal? type "white") "black" "white"))
  
  (define percent-endgame 
    (- 1 (min (/ (- (+ 980 net-white-material) (+ white-material-queens white-material-pawns)) (- one-side-material-at-start 800))
              (/ (- (+ 980 net-black-material) (+ black-material-queens black-material-pawns)) (- one-side-material-at-start 800)))))
  ;(display "Endgame ") (display percent-endgame) (newline)
  
  (define (knights-close-to-center)
    (define (it-over-knights k-list)
      (foldr (lambda (x y) 
               (+ y (let* ([file (car x)]
                           [rank (cdr x)]
                           [posn-cur (cons file rank)]) 
                      (min (manhattan-distance (cons 4 4) posn-cur) 
                           (manhattan-distance (cons 4 5) posn-cur) 
                           (manhattan-distance (cons 5 4) posn-cur) 
                           (manhattan-distance (cons 5 5) posn-cur))))) 0 k-list))
    (* (/ 5 4) (- (it-over-knights black-knights) (it-over-knights white-knights))))
  ;MEASURE OF PROXIMITY OF KNIGHT TO THE CENTER- CLOSER THE BETTER
  ;(display "Knight centre proximity ") (display (knights-close-to-center)) (newline)
  
  
  (define (queen-enemy-king-taxicab-distance) 
    (define (it-over-queens q-list king)
      (foldr (lambda (x y)
               (+ y (manhattan-distance x king))) 0 q-list))
    (/ (- (it-over-queens white-queens black-king) (it-over-queens black-queens white-king)) 2))
  ;QUEENS ARE PENALIZED FOR TAXICAB DISTANCE TO THE ENEMY KING
  ;(display "Queen-king taxicab ") (display (queen-enemy-king-taxicab-distance)) (newline)
  
  
  (define (rook-enemy-king-taxicab-distance)
    (define (it-over-rooks r-list king)
      (foldr (lambda (x y)
               (+ y (manhattan-distance x king))) 0 r-list))
    (/ (- (it-over-rooks white-rooks black-king) (it-over-rooks black-rooks white-king)) 2))
  ;ROOKS ARE PENALIZED FOR TAXICAB DISTANCE TO THE ENEMY KING
  ;(display "Rook-king taxicab ") (display (rook-enemy-king-taxicab-distance)) (newline)
  
  
  (define (knights-radius-of-two)
    (define (one-knight posn type)
      (let* ([k-file (car posn)]
             [k-rank (cdr posn)]
             [indices (lc (cons x y): x <- (range (max 1 (- k-file 2)) (min 9 (+ k-file 3))) y <- (range (max 1 (- k-rank 2)) (min 9 (+ k-rank 3)))
                          * (if (equal? #f (2d-vector-ref fboard (- x 1) (- y 1))) 
                                #f 
                                (not (equal? (piece-color (2d-vector-ref fboard (- x 1) (- y 1))) type))))])
        (length indices)))
    (define (all-knights k-list type)
      (foldr (lambda (x y) (+ y (one-knight x type))) 0 k-list))
      (* 4 percent-endgame (- (all-knights white-knights "white") (all-knights black-knights "black"))))
  ;MEASURE OF NUMBER OF PIECES WITHIN 2 SQUARES OF KNIGHT, ALSO TAKES INTO ACCOUNT ENDGAME CONSIDERATIONS
  ;(display "Knight-two-radius ") (display (knights-radius-of-two)) (newline)
  
  
  (define (kings-center)
    (define (one-king posn type)
      (let* ([file (car posn)]
             [rank (cdr posn)]
             [posn-cur (cons file rank)]
             [is-at-center? (if (< (min (chebyshev-distance (cons 4 4) posn-cur) 
                                        (chebyshev-distance (cons 4 5) posn-cur) 
                                        (chebyshev-distance (cons 5 4) posn-cur) 
                                        (chebyshev-distance (cons 5 5) posn-cur)) 2) 1 0)]
             [not-just-pawns? (if (equal? "white" type)
                                  (if (= net-black-material black-material-pawns) 0 1)
                                  (if (= net-white-material white-material-pawns) 0 1))])
        (* not-just-pawns? is-at-center? (- (* 60 percent-endgame) 24))))
    (- (one-king white-king "white") (one-king black-king "black")))
  ;KINGS GET A PENALTY-BONUS FOR BEING IN THE CENTER
  ;(display "King centre proximity ") (display (kings-center)) (newline)
  
  
  (define (bishop-endgame-bonus)
    (* 10 percent-endgame (- (length white-bishops) (length black-bishops))))
  ;BISHOP GETS A BONUS FOR EXISTING TOWARDS THE END OF THE GAME
  ;(display "Bishop endgame bonus ") (display (bishop-endgame-bonus)) (newline)
  
  
  (define (pawns-on-rook-file)
    (define (one-rook posn type)
      (define any-same-pawn-found? 1)
      (define any-opp-pawn-found? 1)
      (define (iter i)
        (if (= i 9) (+ (* 10 any-same-pawn-found?) (* 4 any-opp-pawn-found?))
            (let* ([piece-here (2d-vector-ref fboard (- (car posn) 1) (- i 1))]) 
              (cond [(equal? #f piece-here) (iter (+ 1 i))]
                    [(and (not (= i (cdr posn))) 
                          (equal? (piece-type piece-here) "P") 
                          (not (equal? (piece-color piece-here) type))) 
                     (begin (set! any-opp-pawn-found? 0) (iter (+ 1 i)))]
                    [(and (not (= i (cdr posn))) 
                          (equal? (piece-type piece-here) "P") 
                          (equal? (piece-color piece-here) type)) 
                     (begin (set! any-same-pawn-found? 0) (iter (+ 1 i)))]
                    [else (iter (+ 1 i))]))))
      (iter 1))
    (define (all-rooks rooklist type)
      (foldr (lambda (x y) (+ y (one-rook x type))) 0 rooklist))
    (- (all-rooks white-rooks "white") (all-rooks black-rooks "black")))
  ;BONUS BASED ON THE NUMBER OF SELF/ENEMY PAWNS ON THE FILE OCCUPIED BY THE ROOK
  ;(display "Pawn on rook file ") (display (pawns-on-rook-file)) (newline)
  
  (define (bishop-at-adge)
    (define (closest-edge-dist posn)
      (min (abs (- 8 (car posn))) (abs (- 1 (car posn))) (abs (- 8 (cdr posn))) (abs (- 1 (cdr posn)))))
    (define (all-bishops bishop-list)
      (foldr (lambda (x y) (+ y (+ 14 (* 3 (closest-edge-dist x))))) 0 bishop-list))
    (- (all-bishops white-bishops) (all-bishops black-bishops)))
  ;BONUS BASED ON A BISHOP BEING AT AN EDGE OR IN THE CENTER
  ;(display "Centered bishop bonus ") (display (bishop-at-adge)) (newline)
  
  (define (knight-king-distance)
    (define (one-knight posn king-pos)
      (/ (manhattan-distance posn king-pos) 3))
    (define (all-knights knight-list)
      (foldr (lambda (x y) (+ y (one-knight x white-king) (one-knight x black-king))) 0 knight-list))
    (- (all-knights black-knights) (all-knights white-knights)))
  ;PROXIMITY OF KNIGHT TO EITHER KING- THIS IS ALWAYS NEGATIVE, HENCE THE REVERSAL.
  ;(display "Knight king distance ") (display (knight-king-distance)) (newline)
  
  (define (is-this-pawn-isolated? posn type)
    (let* ([file (car posn)])
      (if (equal? type "white")
          (if (null? (lc x : x <- white-pawns * (or (equal? (car x) (- file 1)) 
                                                    (equal? (car x) (+ file 1))))) #t #f)
          (if (null? (lc x : x <- black-pawns * (or (equal? (car x) (- file 1)) 
                                                    (equal? (car x) (+ file 1))))) #t #f))))
  ;TO CHECK IF A PAWN WITH GIVEN TYPE AND POSN IS ISOLATED OR NOT
  
  
  (define (isolated-pawns)
    (define (one-type-pawn-list p-list type)
      (foldr (lambda (x y) (+ y (cond [(not (is-this-pawn-isolated? x type)) 0]
                                      [(or (equal? (car x) 1) (equal? (car x) 8)) 12]
                                      [(or (equal? (car x) 2) (equal? (car x) 7)) 14]
                                      [(or (equal? (car x) 3) (equal? (car x) 6)) 16]
                                      [(or (equal? (car x) 4) (equal? (car x) 5)) 20]))) 0 p-list))
    (- (one-type-pawn-list black-pawns "black") (one-type-pawn-list white-pawns "white")))
  ;TO PENALIZE ISOLATED PAWNS, HENCE THE REVERSAL
  ;(display "Isolated pawn penalty ") (display (isolated-pawns)) (newline)
  
  
  (define (doubled-pawn-files)
    (define (doubled-pawn-files-one-color type)
      (define (for-each-file file)
        (if (= file 9) 0
            (+ (for-each-file (+ file 1)) (let* ([num-pawns-in-file (length (lc x : 
                                                                                x <- (if (equal? type "white") white-pawns black-pawns) 
                                                                                * (equal? (car x) file)
                                                                                * (not (is-this-pawn-isolated? x type))))]) 
                                            (if (< 1 num-pawns-in-file) (* 6 num-pawns-in-file) 0)))))
      (for-each-file 1))
    (- (doubled-pawn-files-one-color "black") (doubled-pawn-files-one-color "white")))
  ;TO IDENTIFY THE DOUBLED PAWN FILES
  ;(display "Doubled pawns ") (display  (doubled-pawn-files)) (newline)
   
   
  
  (define (d2-e2-pawn-penalty)
    (let* ([which-pawn-on-d2? (if (and (not (equal? #f (2d-vector-ref fboard 3 1))) 
                                       (equal? (piece-type (2d-vector-ref fboard 3 1)) "P")
                                       (equal? (piece-color (2d-vector-ref fboard 3 1)) "white")) 
                                  10 0)]
           [which-pawn-on-e2? (if (and (not (equal? #f (2d-vector-ref fboard 4 1))) 
                                       (equal? (piece-type (2d-vector-ref fboard 4 1)) "P")
                                       (equal? (piece-color (2d-vector-ref fboard 4 1)) "white")) 
                                  10 0)]
           [which-pawn-on-d7? (if (and (not (equal? #f (2d-vector-ref fboard 3 6))) 
                                       (equal? (piece-type (2d-vector-ref fboard 3 6)) "P")
                                       (equal? (piece-color (2d-vector-ref fboard 3 6)) "black")) 
                                  10 0)]
           [which-pawn-on-e7? (if (and (not (equal? #f (2d-vector-ref fboard 4 6))) 
                                       (equal? (piece-type (2d-vector-ref fboard 4 6)) "P")
                                       (equal? (piece-color (2d-vector-ref fboard 4 6)) "black")) 
                                  10 0)])
      (- (+ which-pawn-on-d7? which-pawn-on-e7?) (+ which-pawn-on-d2? which-pawn-on-e2?))))
  ;NET PENALTY ON PAWNS ON D2 AND E2 FOR WHITE, D7 AND E7 FOR BLACK
  ;(display "D2 E2 Pawn penalty ") (display (d2-e2-pawn-penalty)) (newline)
  
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;YAHAN SAGAR AUR MERA CODE MERGE HOGA
  
  (define (pawn-advancement)
    (define (one-pawn-list type plist)
      (if (equal? type "white")
          (foldr (lambda (x y) (+ y (let* ([file (car x)]
                                       [rank (cdr x)]
                                       [factor (- 4 (abs (min (- 4 file) (- 5 file))))]) (* factor rank)))) 0 plist)
          (foldr (lambda (x y) (+ y (let* ([file (car x)]
                                       [rank (- 8 (cdr x))]
                                       [factor (- 4 (abs (min (- 4 file) (- 5 file))))]) (* factor rank)))) 0 plist)))
    (- (one-pawn-list "white" white-pawns) (one-pawn-list "black" black-pawns)))
  ;(display "Pawn advancement ") (display (pawn-advancement)) (newline)
  
  (define (bishop-mobility-bonus)
    (define (one-type-bishop type blist)
      (foldr (lambda (x y) (+ y (+ -4 (/ (* 22 (min 12 (length (send (new bishop%) give-all-moves board-pos type x)))) 12)))) 0 blist))
    (- (one-type-bishop "white" white-bishops) (one-type-bishop "black" black-bishops)))
  ;(display "Bishop mobility bonus ") (display (bishop-mobility-bonus)) (newline)
  
  (define (rook-mobility-bonus)
    (define (one-type-rook type blist)
      (foldr (lambda (x y) (+ y (/ (* 20 (min 12 (length (send (new rook%) give-all-moves board-pos type x)))) 12))) 0 blist))
    (- (one-type-rook "white" white-rooks) (one-type-rook "black" black-rooks)))
  ;(display "Rook mobility bonus ") (display (rook-mobility-bonus)) (newline)
  
  (define (king-open-files)
    (define open-files (lc file : file <- (range 1 9)
                           * (null? (flatten (lc pawn : pawn <- white-pawns
                                                 * (eq? (car pawn) file))))
                           * (null? (flatten (lc pawn : pawn <- black-pawns
                                                 * (eq? (car pawn) file))))))
    (define white-open-files (lc file : file <- (range 1 9)
                                 * (null? (flatten (lc pawn : pawn <- white-pawns
                                                       * (eq? (car pawn) file))))))
    (define black-open-files (lc file : file <- (range 1 9)
                                 * (null? (flatten (lc pawn : pawn <- black-pawns
                                                       * (eq? (car pawn) file))))))
    (define total-score 0)
    
    (let* ([white-king-file (car white-king)]
           [black-king-file (car black-king)]
           [white-king-side-file (if (< (abs (- 8 (min 8 (+ 1 white-king-file))))
                                     (abs (- 1 (max 1 (- white-king-file 1))))) (min 8 (+ 1 white-king-file)) 
                                                                                (max 1 (- white-king-file 1)))]
           [black-king-side-file (if (< (abs (- 8 (min 8 (+ 1 black-king-file))))
                                     (abs (- 1 (max 1 (- black-king-file 1))))) (min 8 (+ 1 black-king-file)) 
                                                                                (max 1 (- black-king-file 1)))])
      (begin (if (or (member white-king-file open-files) (member white-king-file white-open-files))
                 (set! total-score (- total-score (* 23 (- 1 percent-endgame)))) (void)) 
             (if (or (member black-king-file open-files) (member black-king-file black-open-files))
                 (set! total-score (+ total-score (* 23 (- 1 percent-endgame)))) (void)) 
             total-score)))
      ;(display "Open king files ") (display (king-open-files)) (newline)
  
  (define (pawns-within-2-distance-to-king)
    (define (one-type pawn-list king-posn)
      (foldr (lambda (x y) (+ y (if (< 5 (+ (chebyshev-distance x king-posn) (manhattan-distance x king-posn))) 10 0))) 0 pawn-list))
    (- (one-type white-pawns black-king) (one-type black-pawns white-king)))
  
  ;(display "Pawns within 2 steps of king ") (display (pawns-within-2-distance-to-king)) (newline)
  
  (define (knights-not-driven-away)
    (define (posn-list-for-one-enemy-pawn k-type)
      (flatten (foldr (lambda (x y) (append y (send (new pawn%) give-all-moves board-pos (other-type k-type) x)))
             '() (if (equal? k-type "white") black-pawns white-pawns))))
    (define (is-the-knight-threatened? knight-pos knight-type)
      (if (member knight-pos (posn-list-for-one-enemy-pawn knight-type)) #t #f))
    (define (num-nonthreatened-knights type)
      (foldr (lambda (x y) (+ y (if (is-the-knight-threatened? x type) 0 8))) 
             0 
             (if (equal? type "white") white-knights black-knights)))
    (* percent-endgame (- (num-nonthreatened-knights "white") (num-nonthreatened-knights "black"))))
  
  ;(display "Knights not driven away ") (display (knights-not-driven-away)) (newline)
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
   (define (backward-pawn-penalty)
    
    (define white-backward-pawns
      (lc pawn : pawn <- white-pawns
	  * (let* ([file (car pawn)]
		   [rank (cdr pawn)])
	      (null? (flatten
		      (lc p : p <- white-pawns
			  * (or (eq? (car p) (+ file 1))
				(eq? (car p) (- file 1)))
			  * (<= (cdr p) rank)))))))
    
    (define white-backward-pawns-half-open-file
      (lc pawn : pawn <- white-backward-pawns
	  * (let* ([file (car pawn)]
		   [rank (cdr pawn)])
	      (null? (flatten
		      (lc p : p <- black-pawns
			  * (eq? (car p) file)
			  * (>= (cdr p) rank)))))))

    (define black-backward-pawns
      (lc pawn : pawn <- black-pawns
	  * (let* ([file (car pawn)]
		   [rank (cdr pawn)])
	      (null? (flatten
		      (lc p : p <- black-pawns
			  * (or (eq? (car p) (+ file 1))
				(eq? (car p) (- file 1)))
			  * (>= (cdr p) rank)))))))

    (define black-backward-pawns-half-open-file
      (lc pawn : pawn <- black-backward-pawns
	  * (let* ([file (car pawn)]
		   [rank (cdr pawn)])
	      (null? (flatten
		      (lc p : p <- white-pawns
			  * (eq? (car p) file)
			  * (<= (cdr p) rank)))))))
    
    (define (calc-num-attacks half-board-position pawns-list)
      (foldr (lambda (one-piece-positions num-attacks)
	       (+ (foldr (lambda (pos local-num-attacks)
			   (+ (if (member pos pawns-list)
				  1 0) local-num-attacks))
			 0 (cdr one-piece-positions))
		  num-attacks))
	     0 half-board-position))
    
    (define num-attacks-on-white-backward-pawns
      (foldr (lambda (one-position num-attacks)
	       (+ (calc-num-attacks (cdr one-position) white-backward-pawns)
		  num-attacks))
	     0 all-moves-possible-by-black))

    (define num-attacks-on-black-backward-pawns
      (foldr (lambda (one-position num-attacks)
	       (+ (calc-num-attacks (car one-position) black-backward-pawns)
		  num-attacks))
	     0 all-moves-possible-by-white))

    (+ (* 6 (length black-backward-pawns)) 
       (- (* 6 (length white-backward-pawns)))
       (* 4 (length black-backward-pawns-half-open-file))
       (- (* 4 (length white-backward-pawns-half-open-file)))
       (* 4 num-attacks-on-black-backward-pawns)
       (- (* 4 num-attacks-on-white-backward-pawns))))
  
  ;(display "backward-pawn-penalty ") (display (backward-pawn-penalty)) (newline)

  
  ;; (define (bishop-xray-mobility)
  ;;   (define bishop-xray-white
  ;;     (send this filter-posn pair-position
  ;; 		    (lc (cons (+ file x) (+ rank x)) : x <- move-range 
  ;; 			* (> (+ file x) 0) * (> (+ rank x) 0) 
  ;; 			* (< (+ file x) 9) * (< (+ rank x) 9)))
  ;;   (+ (* 8 (length bishop-xray-white))
  ;;      (- (* 8 (length bishop-xray-black)))))
  ;; (display "bishop-xray-mobility ") (display (bishop-xray-mobility)) (newline)



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;SOMETHING RAGHAV WROTE
    (define (white-attacks-here x)
      (+ (length (foldr (lambda (x y) (lc apos : apos <- (send (new pawn%) give-all-moves board-pos "white" x)
                                          * (and (equal? apos x) #t))) '() white-pawns))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new bishop%) give-all-moves board-pos "white" x)
                                          * (and (equal? apos x) #t))) '() white-bishops))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new rook%) give-all-moves board-pos "white" x)
                                          * (and (equal? apos x) #t))) '() white-rooks))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new knight%) give-all-moves board-pos "white" x)
                                          * (and (equal? apos x) #t))) '() white-knights))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new queen%) give-all-moves board-pos "white" x)
                                          * (and (equal? apos x) #t))) '() white-queens))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new king%) give-all-moves board-pos "white" x)
                                          * (and (equal? apos x) #t))) '() (list white-king)))))
    ;TO DETERMINE THE NUMBER OF WHITE PIECES ATTACKING A GIVEN POSITION
    (define (black-attacks-here x)
      (+ (length (foldr (lambda (x y) (lc apos : apos <- (send (new pawn%) give-all-moves board-pos "black" x)
                                          * (and (equal? apos x) #t))) '() black-pawns))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new bishop%) give-all-moves board-pos "black" x)
                                          * (and (equal? apos x) #t))) '() black-bishops))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new rook%) give-all-moves board-pos "black" x)
                                          * (and (equal? apos x) #t))) '() black-rooks))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new knight%) give-all-moves board-pos "black" x)
                                          * (and (equal? apos x) #t))) '() black-knights))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new queen%) give-all-moves board-pos "black" x)
                                          * (and (equal? apos x) #t))) '() black-queens))
         (length (foldr (lambda (x y) (lc apos : apos <- (send (new king%) give-all-moves board-pos "black" x)
                                          * (and (equal? apos x) #t))) '() (list black-king)))))
    ;TO DETERMINE THE NUMBER OF BLACK PIECES ATTACKING A GIVEN POSITION
    
    (define (num-checks)
      (define (num-attacks-on-white-on-pos posn)
        (+ (length (foldr (lambda (x y) (lc apos : apos <- (send (new pawn%) give-all-moves board-pos "black" x)
                                            * (and (equal? apos posn) (= 0 (white-attacks-here x))))) '() black-pawns))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new bishop%) give-all-moves board-pos "black" x)
                                            * (and (equal? apos posn) (= 0 (white-attacks-here x))))) '() black-bishops))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new rook%) give-all-moves board-pos "black" x)
                                            * (and (equal? apos posn) (= 0 (white-attacks-here x))))) '() black-rooks))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new knight%) give-all-moves board-pos "black" x)
                                            * (and (equal? apos posn) (= 0 (white-attacks-here x))))) '() black-knights))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new queen%) give-all-moves board-pos "black" x)
                                            * (and (equal? apos posn) (= 0 (white-attacks-here x))))) '() black-queens))))
      (define (num-attacks-on-black-on-pos posn)
        (+ (length (foldr (lambda (x y) (lc apos : apos <- (send (new pawn%) give-all-moves board-pos "white" x)
                                            * (and (equal? apos posn) (= 0 (black-attacks-here x))))) '() white-pawns))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new bishop%) give-all-moves board-pos "white" x)
                                            * (and (equal? apos posn) (= 0 (black-attacks-here x))))) '() white-bishops))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new rook%) give-all-moves board-pos "white" x)
                                            * (and (equal? apos posn) (= 0 (black-attacks-here x))))) '() white-rooks))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new knight%) give-all-moves board-pos "white" x)
                                            * (and (equal? apos posn) (= 0 (black-attacks-here x))))) '() white-knights))
           (length (foldr (lambda (x y) (lc apos : apos <- (send (new queen%) give-all-moves board-pos "white" x)
                                            * (and (equal? apos posn) (= 0 (black-attacks-here x))))) '() white-queens))))
      (* 50 (- (num-attacks-on-white-on-pos black-king) (num-attacks-on-white-on-pos white-king))))
    ;(display "Num safe checks ") (display (num-checks)) (newline)
  
    (define (pawn-promotion-bonus)
      (define (one-black-pawn pawn-pos)
        (let* ([next-pos-pawn (send (new pawn%) give-all-moves board-pos "black" pawn-pos)]
               [pawn-score (foldr (lambda (x y) (+ y (if (= 0 (white-attacks-here x)) 1 0))) 0 next-pos-pawn)])
          (if (> pawn-score 0) (/ (+ (* 14 (- 8 (cdr pawn-pos)) (- 8 (cdr pawn-pos))) 4) 3) 0)))
      (define (one-white-pawn pawn-pos)
        (let* ([next-pos-pawn (send (new pawn%) give-all-moves board-pos "white" pawn-pos)]
               [pawn-score (foldr (lambda (x y) (+ y (if (= 0 (black-attacks-here x)) 1 0))) 0 next-pos-pawn)])
          (if (> pawn-score 0) (/ (+ (* 14 (cdr pawn-pos) (cdr pawn-pos)) 4) 3) 0)))
      (define black-pawn-score
        (foldr (lambda (x y) (+ y (one-black-pawn x))) 0 black-pawns))
      (define white-pawn-score
        (foldr (lambda (x y) (+ y (one-white-pawn x))) 0 white-pawns))
      (- white-pawn-score black-pawn-score))
    ;(display "Pawn promotion bonus ") (display (pawn-promotion-bonus)) (newline)
          
  
    (begin (place-all-pieces)
           (+ net-total-material (knights-close-to-center) (queen-enemy-king-taxicab-distance)
              (rook-enemy-king-taxicab-distance) (knights-radius-of-two) (kings-center)
              (bishop-endgame-bonus) (pawns-on-rook-file) (bishop-at-adge)
              (knight-king-distance) (isolated-pawns) (doubled-pawn-files) (d2-e2-pawn-penalty)
              (pawn-advancement) (bishop-mobility-bonus) (rook-mobility-bonus)
              (king-open-files) (pawns-within-2-distance-to-king) (knights-not-driven-away)
              (backward-pawn-penalty) (num-checks) (pawn-promotion-bonus))))

  
;(define x (new complete-position% (board-position random-complete-position) (move (cons 0 0)) (turn "white") (scew #t) (sceb #t) (lcew #t) (lceb #t)))
  
  (define (recurse i)
    (if (eq? i 50)
        (void)
        (begin
          (evaluate-posn random-complete-position-2)
          (recurse (+ i 1)))))