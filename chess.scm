;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.01a   2025-12-28    A very first draft
;;                                Initialising things that could be of good use
;;  version 0.01b   2025-12-29    Piece and board data and pretty printing
;;  version 0.01c   2025-12-29    Start to create lists for all possible moves for any given board setup
;;  version 0.01d   2025-12-30    More work on creating lists for all possible moves for any given board setup
;;  version 0.01e   2025-12-30    All moves for individual pieces. (except en-passant)
;;  version 0.01f   2025-12-30    All moves for individual pieces. Added 'list-moves' function
;;  version 0.01g   2025-12-31    Refactoring the 'all-moves' function, small changes and starting the game loop
;;  version 0.01h   2026-01-03    Working on the 'make the move' functions - bugs in the moves generator :-(
;;  version 0.01i   2026-01-03    'make the move' functions look all 0K. - check on board dimensions
;;  version 0.01j   2026-01-04    Added 'Check' test
;;  version 0.01k   2026-01-04    Added 'Checkmate' test - rewrote (list-moves) for readable output
;;  version 0.01l   2026-01-06    Issues with game-loop . . .
;;                                The 'all-moves' function is not complete correct.
;;                                The King can move to 'check' position - check for free positions
;;  version 0.01m   2026-01-07    Adding a test whether after a player's move the player's King is not checked.
;;                                Refactor a lot on the 'all-moves' and 'all-moves-list' functions
;;  version 0.01n   2026-01-08    Some minor changes and removing obsolete code
;;  version 0.01o   2026-01-09    Work on the opening library
;;
;;
;; W.T.D.: Think about valuating a board position - then write the function...
;;         Add creating a list of moves in official chess notation
;;         Then... start the enigine with 'mate in 2' samples
;;
;;
;;  (cl) 2025-12-31, 2026-01-09 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Info on chess
;;
;; Chess notation, primarily Algebraic Notation, is a system to record moves using piece initials
;; (K, Q, R, B, N for King, Queen, Rook, Bishop, Knight; pawns have none), destination square coordinates
;; (e.g., e4), 'x' for captures, '+' for check, '#' for checkmate, 'O-O'/'O-O-O' for castling,
;; and '=' for promotion (e.g., e8=Q).
;;
                                                                                                 
;; Using the 'scheme' language in DrRacket
;;
#lang scheme

(require "chess-pieces.scm")
(require "boards.scm")
(require "opening-library.scm")


(define CheckMate 42)
(define Quit      99)
(define QuitGame (list (list Quit Quit) (list Quit Quit))) ;; format as a move
                       
(define NoMoves '())

(define A 1) ;; Some board helpers
(define B 2)
(define C 3)
(define D 4)
(define E 5)
(define F 6)
(define G 7)
(define H 8)

;; Some helper functions for better readability
(define (first lst)  (car    lst))
(define (second lst) (cadr   lst))
(define (third lst)  (caddr  lst))
(define (fourth lst) (cadddr lst))
(define (rest lst)   (cdr    lst))
(define (nth lst n)  (list-ref lst n))

;; The sgn-function is not available in MIT-Scheme
(define (sgn x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

(define (colour piece-value) (sgn piece-value))

(define (negate x) (- x))

(define (opponents-colour players-colour) (- players-colour))

;; All pretty functions convert board & piece data to strings
(define (pretty-colour piece-value)
  (if (= empty piece-value)
      " "
      (if (= (colour piece-value) white)
          "w"
          "b")))

(define (absolute-piece-value piece-value)
  (modulo (abs piece-value) 10))

;; (case) won't work here and on more places, so going for a (cond) list
;; very readable
(define (pretty-type piece-value)
  (let ((apv (absolute-piece-value piece-value)))
        (cond ((= apv King)   "K")
              ((= apv Queen)  "Q")
              ((= apv Bishop) "B")
              ((= apv Knight) "N")
              ((= apv Rook)   "R")
              ((= apv Pawn)   "p")
              (else           "_"))))              

(define (pretty-state piece-value)
  (let ((pst (get-piece-state piece-value)))
        (cond ((= pst First-Move) "1")
              ((= pst En-Passant) "x")
              ((= pst Castling)   "%")
              (else               " "))))
              
(define (pretty-piece piece-value)
  (let ((piece-colour (pretty-colour piece-value))
        (piece-type   (pretty-type   piece-value))
        (piece-state  (pretty-state  piece-value)))
        (string-append piece-colour piece-type piece-state)))

(define (pretty-line-of-pieces piece-values)
  (if (null? piece-values)
      "\n\n"
      (string-append (pretty-piece (first piece-values))
                     "  "
                     (pretty-line-of-pieces (rest piece-values)))))

(define (pretty-board board line)
  (if (null? board)
      ""
      (string-append " "
                     (number->string line)
                     "   "
                     (pretty-line-of-pieces (first board))
                     (pretty-board (rest board) (- line 1)))))

(define (display-correct-board board)
  (display "\n\n\n               The current board\n\n\n")
  ;; reverse so the first row is at the bottom 
  (display (pretty-board (reverse board) 8))
  (display "      a    b    c    d    e    f    g    h\n\n\n"))
      
;; Convert the board data to a string
(define (display-board board)
  (if (and (= (length board) 8)
           (= (apply + (map length board)) 64))
      (display-correct-board board)
      (display "Incorrect 'board' data...\n\n")))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;; The readable list of all possible moves
;;

(define (pretty-colour-plus piece-colour)
  (if (= piece-colour white) "white" "black"))

(define (pretty-type-plus piece-type)
  (cond ((= piece-type King)   "King  ")
        ((= piece-type Queen)  "Queen ")
        ((= piece-type Bishop) "Bishop")
        ((= piece-type Knight) "Knight")
        ((= piece-type Rook)   "Rook  ")
        ((= piece-type Pawn)   "Pawn  ")
        (else                  " _    ")))
                          
(define (pretty-piece-plus piece-value)
  (let ((piece-colour (pretty-colour-plus (colour piece-value)))
        (piece-type   (pretty-type-plus (absolute-piece-value piece-value))))
    (string-append piece-colour " " piece-type)))

(define (pretty-move move)
  (let ((move-letter (string (integer->char (+ 96 (first  move)))))
        (move-number (string (integer->char (+ 48 (second move))))))
    (string-append move-letter move-number)))

(define (equal-piece base-move test-move)
  (equal? (first base-move) (first test-move)))

(define (non-equal-piece base-move test-move)
  (not (equal-piece base-move test-move)))

(define (sweep-by-piece moves)
  (if (null? moves)
      NoMoves
      (let ((from-piece (first (first moves)))
            (from-moves (filter (lambda (t) (equal-piece     (first moves) t)) moves))
            (rest-moves (filter (lambda (t) (non-equal-piece (first moves) t)) moves)))
        (cons
         (cons from-piece (map second from-moves))
         (sweep-by-piece rest-moves)))))

(define (add-piece-value board moves)
  (let ((x (first  (first moves)))
        (y (second (first moves))))
    (cons
     (cons (location-value board x y)
           (list (first moves)))
     (rest moves))))
          
(define (add-piece-values board sweeped-moves)
  (map (lambda (moves) (add-piece-value board moves)) sweeped-moves))

(define (sweep board moves)
  (add-piece-values board (sweep-by-piece moves)))

(define (pretty-moves-plus from-move to-moves)
  (if (null? to-moves)
      ""
      (string-append from-move "-" (first to-moves) "  "
                     (pretty-moves-plus from-move (rest to-moves)))))

;;; List for one piece
(define (pretty-list-moves moves)
  (let ((piece-description (pretty-piece-plus (first (first moves))))
        (from-move  (pretty-move (second (first moves))))
        (to-moves   (map pretty-move (rest moves))))
    (string-append piece-description " :  " (pretty-moves-plus from-move to-moves) "\n")))

;;; A bit pretty print of all moves
(define (list-moves moves)
  (display (apply string-append (map pretty-list-moves moves))))


;; Reset the Castling for a given colour
;;
(define (column-erase-can-castling-states piece-value piece-colour)
  (if (and (= (colour piece-value) piece-colour)
           (can-do-Castling piece-value))
      (get-piece-type piece-value)
      piece-value))
      
(define (row-erase-can-castling-states row-pieces piece-colour)
  (map (lambda (piece) (column-erase-can-castling-states piece piece-colour)) row-pieces))

(define (erase-can-castling-states board piece-colour)
  (map (lambda (pieces) (row-erase-can-castling-states pieces piece-colour)) board))

;; En-passant is only valid for one move
;;
(define (column-erase-en-passant-states piece-value)
  (if (can-take-En-Passant piece-value)
      (get-piece-type piece-value)
      piece-value))
      
(define (row-erase-en-passant-states row-pieces)
  (map (lambda (piece) (column-erase-en-passant-states piece)) row-pieces))

(define (erase-en-passant-states board)
  (map (lambda (pieces) (row-erase-en-passant-states pieces)) board))

;; Next step is to generate a list of all possible moves on a board for the player at move
;; First - generate a list of all possible moves for a given location on the board
;; Locations on the chess board are a1..h8
;; In this code for 'a' through 'h' we use 1 through 8, so e2 is entered as (5 2)

(define (get-piece-type piece-value)
  (* (colour piece-value) (absolute-piece-value piece-value)))

(define (get-piece-state piece-value)
  (* 10 (quotient (abs piece-value) 10)))

(define (can-take-En-Passant piece-value)
  (= En-Passant (get-piece-state piece-value)))

(define (can-do-Castling piece-value)
  (= Castling (get-piece-state piece-value)))

(define (location-value board x y)
  (nth (nth board (- y 1)) (- x 1)))

(define (safe-location-value board x y)
    (if (or (< x 1) (> x 8) (< y 1) (> y 8))
      outside
      (nth (nth board (- y 1)) (- x 1))))

(define (safe-nth data index)
  (if (< (length data) (+ index 1))
      '()
      (nth data index)))

;; A standard check for a move of King or Knight
;; Either move to an empty spot or move for a take of opponents piece 
(define (check-move-King-Knight board x y piece-colour possible-move)
  (let ((next-piece-value
         (safe-location-value board
                              (+ x (first  possible-move))
                              (+ y (second possible-move)))))
    (and (not (= next-piece-value outside))
         (or (= next-piece-value empty)
             (= piece-colour (- (colour next-piece-value)))))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; King moves
;;
;; Only check the standard King moves        
(define (check-king-moves board x y piece-colour possible-moves)
  (filter (lambda (move) (check-move-King-Knight board x y piece-colour move)) possible-moves))

;; Here check the possible castling moves
(define (check-castling-king-moves board x y piece-value)
  (if (= Castling (get-piece-state piece-value))
      (append (if (and (= empty (safe-location-value board (- x 1) y))
                       (= empty (safe-location-value board (- x 2) y))
                       (= empty (safe-location-value board (- x 3) y))
                       (= (piece (colour piece-value) Rook Castling)
                          (safe-location-value board (- x 4) y)))
                  '((-3 0))
                  NoMoves)
              (if (and (= empty (safe-location-value board (+ x 1) y))
                       (= empty (safe-location-value board (+ x 2) y))
                       (= (piece (colour piece-value) Rook Castling)
                          (safe-location-value board (+ x 3) y)))
                  '((2 0))
                  NoMoves))
      NoMoves))
                  
(define (king-moves board x y piece-value)
  (let ((standard-king-moves '( (-1 -1) (0 -1) (1 -1)
                                (-1  0)         (1  0)
                                (-1  1) (0  1) (1  1))))
    (append (check-castling-king-moves board x y piece-value)
            (check-king-moves board x y (colour piece-value) standard-king-moves))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Knight moves
;;
;; Check for all possible Knight moves  
(define (check-knight-moves board x y piece-colour possible-moves)
  (filter (lambda (move) (check-move-King-Knight board x y piece-colour move)) possible-moves))

(define (knight-moves board x y piece-value)
  (let ((all-knight-moves '( (-1 -2) (-1  2) (-2 -1) (-2 1)
                             ( 1 -2) ( 1  2) ( 2 -1) ( 2 1))))
    (check-knight-moves board x y (colour piece-value) all-knight-moves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Pawn moves
;;
;; Check for all possible Pawn moves 
(define (reverse-direction move)
  (map (lambda (x) (negate x)) move))

(define (direct-moves moves direction)
  (if (= direction 1)
      moves
      (map (lambda (move) (reverse-direction move)) moves)))

(define (check-pawn-move board x y move)
  (let ((move-piece-value
         (safe-location-value board
                              (+ x (first  move))
                              (+ y (second move)))))
    (and (not (= move-piece-value outside))
         (= move-piece-value empty))))

;; Standard take check
(define (check-pawn-take board x y piece-colour take)
  (let ((take-piece-value
         (safe-location-value board
                              (+ x (first  take))
                              (+ y (second take)))))
    (and (not (= take-piece-value outside))
         (= piece-colour (- (colour take-piece-value))))))

;; En-passant check
(define (check-pawn-en-passant board x y piece-colour take)
  (let ((white-on-row-5 (and (= piece-colour white) (= y 5)))
        (black-on-row-4 (and (= piece-colour black) (= y 4)))
        (en-passant-piece-value
         (safe-location-value board (+ x (first take)) y))
        (take-piece-value
         (safe-location-value board
                              (+ x (first  take))
                              (+ y (second take)))))
    (if (or white-on-row-5 black-on-row-4)
        (and (not (= take-piece-value outside))
             (= (get-piece-state en-passant-piece-value) En-Passant)
             (= take-piece-value empty))
        #f)))

(define (check-pawn-moves board x y step-moves take-moves piece-colour direction first-move)
  (let ((moves (direct-moves
                (if first-move
                    step-moves
                    (list (first step-moves))) direction))
        (takes (direct-moves take-moves direction)))
    (append (filter (lambda (move) (check-pawn-move board x y move)) moves)
            (filter (lambda (take) (check-pawn-en-passant board x y piece-colour take)) takes)
            (filter (lambda (take) (check-pawn-take board x y piece-colour take)) takes))))

(define (pawn-moves board x y piece-value)
  (let ((step-moves '((0 1) (0 2)))
        (take-moves '((-1 1) (1 1)))
        (piece-colour (colour piece-value))
        (direction  (if (= (colour piece-value) white) 1 -1))
        (first-move (= 1 (abs (quotient piece-value 10)))))
    (check-pawn-moves board x y step-moves take-moves piece-colour direction first-move)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; helper for Queen, Bishop and Rook moves
;;
(define (one-moves-list moves)
  (append
   (safe-nth moves 0)
   (safe-nth moves 1)
   (safe-nth moves 2)
   (safe-nth moves 3)
   (safe-nth moves 4)
   (safe-nth moves 5)
   (safe-nth moves 6)
   (safe-nth moves 7)))

(define (walk-directional-step board x y dx dy step piece-colour)
  (let ((px (+ x (* dx step)))
        (py (+ y (* dy step))))
    (append
     (cond ((= (safe-location-value board px py) outside) NoMoves)
           ((= (safe-location-value board px py) empty)
            (cons (list (* dx step) (* dy step))
                  (walk-directional-step board x y dx dy (+ step 1) piece-colour)))
           ((not (= piece-colour (colour (safe-location-value board px py))))
            (list (list (* dx step) (* dy step))))
           (else NoMoves)))))

(define (moves-per-direction board x y piece-colour direction)
  (let ((dx (first  direction))
        (dy (second direction)))
    (walk-directional-step board x y dx dy 1 piece-colour)))

(define (moves-for-all-directions board x y piece-colour directions)
  (one-moves-list
   (map
    (lambda (direction) (moves-per-direction board x y piece-colour direction))
    directions)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Queen moves
;;
(define (queen-moves board x y piece-value)
  (let ((directions '((-1 -1) (0 -1) (1 -1)
                      (-1  0)        (1  0)
                      (-1  1) (0  1) (1  1)))
        (piece-colour (colour piece-value)))
    (moves-for-all-directions board x y piece-colour directions)))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Bishop moves
;;
(define (bishop-moves board x y piece-value)
  (let ((directions '((-1 -1) (1 -1) (-1  1) (1  1)))
        (piece-colour (colour piece-value)))
    (moves-for-all-directions board x y piece-colour directions )))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Rook moves
;;
(define (rook-moves board x y piece-value)
  (let ((directions '((0 -1) (-1  0) (1  0) (0  1)))
        (piece-colour (colour piece-value)))
    (moves-for-all-directions board x y piece-colour directions )))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for an arbitrary piece on a arbitrary position
;;
(define (moves-for-location board x y)
  (if (or (< x 1) (> x 8) (< y 1) (> y 8))
      NoMoves
      (let ((piece-value (location-value board x y)))
        (define apv (absolute-piece-value piece-value))
        (cond ((= apv King)   (king-moves   board x y piece-value))
              ((= apv Queen)  (queen-moves  board x y piece-value))
              ((= apv Bishop) (bishop-moves board x y piece-value))
              ((= apv Knight) (knight-moves board x y piece-value))
              ((= apv Rook)   (rook-moves   board x y piece-value))
              ((= apv Pawn)   (pawn-moves   board x y piece-value))
              (else           NoMoves)))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for all pieces by colour
;;
;; example: ((3 (2 1)) (1 3) (3 3))
;; Piece value 3 is Knight  -  at (2 1) is b1  -  can move to (1 3) is a3 or (3 3) is c3
;;
(define (transpose x y moves)
  (if (null? moves)
      NoMoves
      (let ((move (first moves))
            (rms  (rest  moves)))
        (cons (list (+ x (first move)) (+ y (second move))) 
              (transpose x y rms)))))

(define (all-moves-x-y board piece-colour x y)
  (let ((piece-value (safe-location-value board x y)))
    (if (or (= piece-value outside)
            (= piece-value empty)
            (not (= piece-colour (colour piece-value))))
        NoMoves
        (let ((next-moves (moves-for-location board x y)))          
          (if (null? next-moves)
              NoMoves
              (cons (list piece-value (list x y))
                    (transpose x y next-moves)))))))

(define (all-moves-x board piece-colour y)
  (do ((x 8 (- x 1))
       (rv '() (cons (all-moves-x-y board piece-colour x y) rv)))
    ((< x 1) rv )))

(define (all-moves-y board piece-colour)
  (do ((y 8 (- y 1))
       (rv '() (append (all-moves-x board piece-colour y) rv)))
    ((< y 1) rv)))

;; This is the function without the King-check test 
(define (all-moves-1 board piece-colour)
  (filter (lambda (moves) (not (null? moves))) (all-moves-y board piece-colour)))

;; Here the list is first created via 'all-moves-1'
;; Next reshaped and checked for King-check-moves
;; As a last step reshaped the the 'moves-per-piece' list
(define (all-moves board players-colour)
  (sweep board (all-moves-list board players-colour)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Create a single list of all moves (from -> to) without piece value
;;
(define (strip-piece-value moves)
  (let ((from (second (first moves)))
        (to   (rest moves)))
    (map (lambda (to-move) (list from to-move)) to)))


(define (players-King-checked board move piece-colour)
  (let ((test-board (make-move board move)))
    (is-checked-by test-board (opponents-colour piece-colour))))

(define (remove-King-checked board move piece-colour)
  (not (players-King-checked board move piece-colour)))

(define (all-moves-list board piece-colour)
  (let ((all-possible-moves
         (apply append
                (map (lambda (moves) (strip-piece-value moves)) (all-moves-1 board piece-colour)))))
    (filter (lambda (move) (remove-King-checked board move piece-colour)) all-possible-moves)))

;; Seperate function for 'Check' and 'Checkmate' testing
;; This is to prevent an endless recursive loop
(define (all-moves-list-check-test board piece-colour)
  (apply append (map (lambda (moves) (strip-piece-value moves)) (all-moves-1 board piece-colour))))

       
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Misc. function for making all the moves on the board
;;

(define (eq-location x1 y1 x2 y2)
  (and (= x1 x2) (= y1 y2)))

(define (empty-location-x board ex ey y)
  (do ((x 8 (- x 1))
       (rv '() (cons (if (eq-location ex ey x y)
                         empty
                         (location-value board x y)) rv)))
    ((< x 1) rv )))

(define (empty-location board ex ey)
  (do ((y 8 (- y 1))
       (rv '() (cons (empty-location-x board ex ey y) rv)))
    ((< y 1) rv)))

(define (fill-location-x board piece-value px py y)
  (do ((x 8 (- x 1))
       (rv '() (cons (if (eq-location px py x y)
                         piece-value
                         (location-value board x y)) rv)))
    ((< x 1) rv )))

(define  (fill-location board piece-value px py)
  (do ((y 8 (- y 1))
       (rv '() (cons (fill-location-x board piece-value px py y) rv)))
    ((< y 1) rv)))

(define (work-next-standard-move board from-x from-y to-x to-y)
  (let ((piece-type (get-piece-type (location-value board from-x from-y)))
        (next-board (empty-location board from-x from-y)))
    (fill-location next-board piece-type to-x to-y)))

(define (set-En-Passant piece-type)
  (if (< piece-type 0)
      (- piece-type En-Passant)
      (+ piece-type En-Passant)))

(define (work-two-step-pawn-move board from-x from-y to-x to-y)
  (let ((piece-type (get-piece-type (location-value board from-x from-y)))
        (next-board (empty-location board from-x from-y)))
    (fill-location next-board (set-En-Passant piece-type) to-x to-y)))

;; WORK the En-Passant move!
(define (work-pawn-en-passant-take board from-x from-y to-x to-y)
  (let ((next-board (work-next-standard-move board from-x from-y to-x to-y)))
    (empty-location next-board to-x from-y)))

(define (work-next-pawn-move board from-x from-y to-x to-y)
  (if (= (abs (- from-x to-x)) 1)
      (work-pawn-en-passant-take board from-x from-y to-x to-y)
      (if (= (abs (- from-y to-y)) 1)
          (work-next-standard-move board from-x from-y to-x to-y)
          (work-two-step-pawn-move board from-x from-y to-x to-y))))

(define (work-castling-move board from-x from-y to-x to-y)
  (define board-1 (work-next-standard-move board from-x from-y to-x to-y))
  (if (= to-x 7)
      (work-next-standard-move board-1 8 from-y 6 to-y)
      (work-next-standard-move board-1 1 from-y 3 to-y)))
  
(define (work-next-king-move board from-x from-y to-x to-y)
  (if (> (abs (- from-x to-x)) 1)
      (work-castling-move board from-x from-y to-x to-y)
      (work-next-standard-move board from-x from-y to-x to-y)))
      

;; Move to piece
;; Change the state of a Pawn after its first move
;; Change the status of a Rook and King of their colour when any of the three have made a move
;;
(define  (work-next-move board piece-type piece-colour from-x from-y to-x to-y)
 (cond ((= piece-type Pawn) (work-next-pawn-move board from-x from-y to-x to-y))
       ((= piece-type King) (work-next-king-move board from-x from-y to-x to-y))
       (else (work-next-standard-move board from-x from-y to-x to-y))))

(define (current-piece-data board x y)
  (let ((piece-value (location-value board x y)))
    (list (get-piece-type piece-value) (get-piece-state piece-value))))
    
;; Move is checked for legality
;; 
(define (make-move board move)
  (if (null? move)
      board
      (let ((from-x  (first  (first  move)))
            (from-y  (second (first  move)))
            (to-x    (first  (second move)))
            (to-y    (second (second move))))
        (define piece-data   (current-piece-data board from-x from-y))
        (define piece-type   (abs (first piece-data)))
        (define piece-colour (colour (first piece-data)))         
        (define piece-state  (second piece-data))
        (define next-board-1 (erase-en-passant-states board))
        (define next-board-2
          (if (or (= piece-type King) (= piece-type Rook))
              (erase-can-castling-states next-board-1 piece-colour)
              next-board-1))
        (work-next-move next-board-2 piece-type piece-colour from-x from-y to-x to-y))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Check if a board position is 'Check'
;;
(define (get-position-x board players-piece y)
  (do ((x 8 (- x 1))
       (rv '() (cons (if (= players-piece (get-piece-type (location-value board x y)))
                         (list x y)
                         '()) rv)))
    ((< x 1) rv )))

(define (get-position board players-piece)
  (first
   (filter (lambda (ppos) (not (null? ppos))) 
           (apply append 
                  (do ((y 8 (- y 1))
                       (rv '() (cons (get-position-x board players-piece y) rv)))
                    ((< y 1) rv))))))

(define (is-checked-by board players-colour)
  (let ((to-moves (map second (all-moves-list-check-test board players-colour)))
        (opponents-king-position (get-position board (* (opponents-colour players-colour) King))))
    (if (member opponents-king-position to-moves)
        #t #f )))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Check if a board position is a 'Checkmate'
;;
;; Assumption that the board position is already a 'check' situation
;; This to 'save' time. But it will always work correct.
;;
(define (single-check-test board players-colour opponents-rescue-moves)
  (if (null? opponents-rescue-moves)
      #t
      (let ((rescue-board (make-move board (first opponents-rescue-moves))))
        (if (is-checked-by rescue-board players-colour)
            (single-check-test board players-colour (rest opponents-rescue-moves))
            #f))))

;; Test all opponents legal moves to see if the 'Check' can be rescued
(define (is-checkmate-by board players-colour)
  (let ((rescue-moves (all-moves-list-check-test board (opponents-colour players-colour))))
    (single-check-test board players-colour rescue-moves)))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Read commands or moves from the keyboard
;;
;; First the simple parse only for format 'e2e4'
;; (for now 'e2e4' is the working format)
;;
;; Helper commands:
;;  b     show current board incl. the colour of the player whose turn it is.
;;  m     show all possible moves for the colour of the player whose turn it is.
;;  M     show all possible moves for the opponent's colour.
;;  Q     quit the game.
;;

(define (move-string-to-list move)
  (list (list (- (char->integer (first  move)) 96)
              (- (char->integer (second move)) 48))
        (list (- (char->integer (third  move)) 96)
              (- (char->integer (fourth move)) 48))))
  
(define (parse-move board players-colour entered-move)
  (let ((move (string->list entered-move)))
    (if (null? move)
        NoMoves
        (let ((cmd (first move)))
          (if (= 4 (length move))
              (move-string-to-list move)
              (cond ((equal? cmd #\Q) QuitGame)
                    ((equal? cmd #\b) (display-board board))
                    ((equal? cmd #\c) (LISP-generated-move board players-colour))
                    ((equal? cmd #\o) (display-opening-library-style board))
                    ((equal? cmd #\O) (display opening-library))
                    ((equal? cmd #\m) (list-moves (all-moves board players-colour)))
                    ((equal? cmd #\M) (list-moves (all-moves board (negate players-colour))))
                    (else              NoMoves)))))))

;; Read, parse and check for legality of the move
;; A legal move will return that move
;; otherwise it will return an empty list if the move is illegal
;;
(define (next-move board players-colour)
  (display "\nNext move for player ")
  (display (pretty-colour-plus players-colour))
  (display ": ")
  (define input-move (read-line))
  (let ((move (parse-move board players-colour input-move))
        (legal-moves (all-moves-list board players-colour)))
    (if (or (equal? move QuitGame) (member move legal-moves))
        move
        NoMoves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Openings library incl. helpers
;;

(define (get-piece-character piece-type)
  (cond ((= piece-type King)   #\K)
        ((= piece-type Queen)  #\Q)
        ((= piece-type Bishop) #\B)
        ((= piece-type Knight) #\N)
        ((= piece-type Rook)   #\R)
        ((= piece-type Pawn)   #\P)
        (else                  #\.)))

(define (convert-piece-for-openings-library piece-value)
  (let ((ptc (get-piece-character (abs (get-piece-type piece-value)))))
    (string
     (if (= (colour piece-value) white)
         (integer->char (+ 32 (char->integer ptc)))
         ptc))))
         
(define (opening-library-style-row board-row)
  (apply string-append
         (map convert-piece-for-openings-library board-row)))

(define (opening-library-style board)
  (apply string-append 
         (reverse (map opening-library-style-row board))))

(define (display-opening-library-style board)
  (display "\n")
  (display (opening-library-style board))
  (display "\n"))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Here the code for a computer / LISP generated legal move
;; Code will first check the 'opening library' - and be aware - there is NO check for legal moves on that library


;; This code is useless, but it will generate legal moves
;;
(define (random-move board players-colour)
  (let ((legal-moves (all-moves-list board players-colour)))
    (nth legal-moves (random (length legal-moves)))))

;; Look up board position in opening library
(define (filter-open-moves ols-board library)
  (map second (filter (lambda (oll) (string=? ols-board (first oll))) library)))

(define (LISP-generated-move board players-colour)
  (let ((open-move (filter-open-moves (opening-library-style board) opening-library)))
    (if (null? open-move)
        (random-move board players-colour)
        (move-string-to-list (string->list (first open-move))))))
        


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Minimal game loop
;;

(define (get-move board players-colour)
  (let ((move (next-move board players-colour)))
    (if (null? move)
        (get-move board players-colour)
        move)))

(define (show-check board players-colour)
  (if (is-checked-by board players-colour)
      (string-append "\n* Check!  by " (pretty-colour-plus players-colour) "\n\n")
      ""))

;; Extra function to show to board with the Checkmate position
(define (check-mate-board board players-colour)
  (display-board board)
  (* players-colour CheckMate))

(define (game-loop board players-colour)
  (display-board board)
  (display (show-check board (opponents-colour players-colour)))
  (let ((move (get-move board players-colour)))
    (if (equal? move QuitGame)
        Quit
        (let ((next-board (make-move board move)))
          (if (is-checkmate-by next-board players-colour)
              (check-mate-board next-board players-colour)
              (game-loop next-board (opponents-colour players-colour)))))))

;; Start the game-loop en show end status
;; Incl. Quit or Checkmate test
(define (game board player-colour)
  (let ((a-player (game-loop board player-colour)))
    (if (= (abs a-player) CheckMate)
        (display (string-append
                  "\n* * *  Checkmate! * * *   by "
                  (pretty-colour-plus (colour a-player))
                  "\nBye bye.\n\n"))
        (display "\nYou quit, bye bye.\n\n"))))




;;
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Testing & debugging . . .
;;

;;
(define (c1) (game initial-board white))
(define b initial-board)



;;
;;
(c1)




;;
;; End of code.
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
