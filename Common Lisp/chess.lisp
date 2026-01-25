;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.01a   2025-12-28    A very first draft. Initialising things that could be of good use.
;;  version 0.01b   2025-12-29    Added piece and board data and pretty printing.
;;  version 0.01c   2025-12-29    Start to create lists for all possible moves for any given board setup.
;;  version 0.01d   2025-12-30    More work on creating lists for all possible moves for any given board setup.
;;  version 0.01e   2025-12-30    All moves for individual pieces, except en-passant.
;;  version 0.01f   2025-12-30    All moves for individual pieces. Added the 'list-moves' function.
;;  version 0.01g   2025-12-31    Refactoring the 'all-moves' function, small changes and starting the game loop.
;;  version 0.01h   2026-01-03    Working on the 'make the move' functions. Srious bugs in the moves generator :-(
;;  version 0.01i   2026-01-03    'make the move' functions all look 0K. Added a check on board dimensions.
;;  version 0.01j   2026-01-04    Added 'Check' test.
;;  version 0.01k   2026-01-04    Added 'Checkmate' test, rewrote 'list-moves' for readable output.
;;  version 0.01l   2026-01-06    Issues with the game-loop. The 'all-moves' function is not complete correct.
;;                                The King can move to 'check' position, so check for free positions.
;;  version 0.01m   2026-01-07    Adding a test whether after a player's move the player's King is not checked.
;;                                Refactored a lot on the 'all-moves' and 'all-moves-list' functions.
;;  version 0.01n   2026-01-08    Some minor changes and removing obsolete code.
;;  version 0.01o   2026-01-09    Added the opening library. Started with two moves only.
;;  version 0.01p   2026-01-10    Debuged Castling. My Casting on Rook was TOO strict.
;;                                A first (not standard) list of moves is created, and at least shown at the end.
;;  version 0.01q   2026-01-10    Added promotion, Queen (major) or Knight (minor). Added some helper information.
;;  version 0.01r   2026-01-11    Bug repair for pawn take move. Added some opening moves and Mate-in-2 boards.
;;  version 0.01s   2026-01-12    Started the evaluator for the board position.
;;  version 0.01t   2026-01-12    Added FEN - first conversion from board to FEN.
;;  version 0.01u   2026-01-14    Complete reformatting opening library and refactoring functions.
;;  version 0.02a   2026-01-15    Starting 'best-move' function (now 1 ply only...)
;;  version 0.02b   2026-01-16    Only open e2-e4 for start-game (not for some 'mate in 2'...)
;;                                Working on the 'deep-search' function.
;;  version 0.02c   2026-01-17    Added some chess puzzles
;;  version 0.02d   2026-01-18    Removing the 'First-move' indicator, one function for a standard move of a piece
;;                                Refactoring of the standard move, erase & fill in one go (one nested loop)
;;                                Added a check for a draw position 
;;  version 0.02e   2026-01-20    Some refactoring - struggles with 'tree-search'
;;  version 0.02f   2026-01-21    More work on the 'tree-search' -- almost working 0K. 
;;  version 1.00a   2026-01-21    'tree-search' working 0K. Up to Mate-in-4 working 0K.
;;  version 1.00b   2026-01-22    Added test boards and some minor changes
;;  version 1.00c   2026-01-22    Added 'mate-in-1' check, refactored functions for more speed
;;                                Changes in Knight-position-bonus, now dependent on players colour
;;  version 1.01a   2026-01-23    The common lisp version - first converion steps
;;  version 1.01b   2026-01-24    The common lisp version - issues with apply, map, reduce and member
;;  version 1.01c   2026-01-25    No correct board evaluation - work on computer move
;;  version 1.01d   2026-01-25    Complete correct working conversion from Scheme to Common Lisp
;;
;;
;;  (cl) 2025-12-31, 2026-01-25 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Info on chess
;;
;; Chess notation, primarily Algebraic Notation, is a system to record moves using piece initials
;;      K, Q, R, B, N for King, Queen, Rook, Bishop, Knight; pawns have none
;;      destination square coordinates (e.g., e4)
;;      'x'            captures
;;      '+'            check
;;      '#'            checkmate
;;      'O-O'/'O-O-O'  castling,
;;      '='            promotion (e.g., e8=Q).
;;
;; Forsyth-Edwards Notation (FEN) is a standard, single-line text format to describe any given chess board position,
;; encoding piece placement (using letters for pieces, numbers for empty squares, slashes for ranks),
;; whose turn it is, castling rights, en passant targets, and move counters.
;;

;; Doesn't do much...
(declaim (optimize (speed 3)      ; ← most important for runtime speed
                   (safety 0)     ; disable bounds/type checks
                   (debug 0)      ; no debugger info
                   (compilation-speed 0)))

(load "chess-initials.lisp")
(load "opening-library.lisp")

;; sbcl --non-interactive --eval '(compile-file "chess.lisp")'

;; Make this mutable
;; 
(defconstant search-depth 3)  ;; mate-in-2
;; (defconstant search-depth 4)  ;; just for a game 
;; (defconstant search-depth 5)  ;; mate-in-3
;; (defconstant search-depth 7)  ;; mate-in-4

(defconstant Checkmate 42)
(defconstant Draw      77)
(defconstant Quit.     99)
(defconstant QuitGame (list (list Quit. Quit.) (list Quit. Quit.))) ;; format as a move

(defconstant NoMoves '())

(defun code-info ()
    (format t "~%~%~A~%~%~A~A~A~%~%" 
        "* * *   a tiny and simple Common Lisp chess engine   * * *"
        "version 1.01d   ("
        search-depth
        " ply)   (cl) 2025-12-31, 2026-01-25  by Arno Jacobs"))

;; Helper for a promotion piece
(defparameter *promotion-piece* Queen)

(defun minor-promotion ()
    (setf *promotion-piece* Knight))

(defun major-promotion ()
    (setf *promotion-piece* Queen))

(defun safe-nth (n lst)
    (if (or (< n 0) (>= n (length lst)))
        '()
        (nth n lst)))

(defun sgn (x)
    (cond   ((plusp  x)  1)
            ((minusp x) -1)
            (t           0)))

(defun colour (piece-value) (sgn piece-value))

(defun negate (x) (- x))

(defun opponents-colour (player-colour) (- player-colour))

(defun absolute-piece-value (piece-value)
    (mod (abs piece-value) 10))

(defun get-piece-type (piece-value)
  (* (colour piece-value) (absolute-piece-value piece-value)))

(defun get-piece-state (piece-value)
  (* 10 (floor (abs piece-value) 10)))

(defun can-take-En-Passant? (piece-value)
  (= En-Passant (get-piece-state piece-value)))

(defun can-do-Castling? (piece-value)
  (= Castling (get-piece-state piece-value)))

;; (case) won't work here and on more places, so going for a (cond) list
;; very readable
(defun pretty-type (piece-value)
    (let ((apv (absolute-piece-value piece-value)))
            (cond   ((= apv King)   "K")
                    ((= apv Queen)  "Q")
                    ((= apv Bishop) "B")
                    ((= apv Knight) "N")
                    ((= apv Rook)   "R")
                    ((= apv Pawn)   "p")
                    (t              "_"))))

(defun pretty-state (piece-value)
    (let ((pst (get-piece-state piece-value)))
            (cond   ((= pst En-Passant) "x")
                    ((= pst Castling)   "%")
                    (t                  " "))))

(defun pretty-colour-on (piece-colour) 
    (if (= piece-colour black)
        (format NIL "~c[102m~c[30m~c[1m" #\ESC #\ESC #\ESC)
        "" ))

(defun pretty-colour-off (piece-colour) 
    (if (= piece-colour black)
        (format NIL "~c[0m" #\ESC)
        "" ))

(defun pretty-piece (piece-value)
    (let ((piece-type   (pretty-type   piece-value))
          (piece-state  (pretty-state  piece-value))
          (piece-colour-on    (pretty-colour-on  (colour piece-value)))
          (piece-colour-off   (pretty-colour-off (colour piece-value))))
        (concatenate 'string piece-colour-on piece-type piece-colour-off piece-state)))

(defun pretty-line-of-pieces (piece-values)
    (when piece-values
        (concatenate 'string 
            (pretty-piece (first piece-values))
            "  "
            (pretty-line-of-pieces (rest piece-values)))))

(defun display-pretty-board-line (board line)
    (format t " ~A    ~A~%" 
        (write-to-string line)
        (pretty-line-of-pieces (first board))))

(defun display-pretty-board (board line)
    (when board
        (display-pretty-board-line board line)
        (display-pretty-board (rest board) (- line 1))))

(defun display-correct-board (board)
    ;; reverse so the first row is at the bottom 
    (format t "~%~%~%~A" "")
    (display-pretty-board (reverse board) height)
    (format t "~%~A~%~%~%" "      a   b   c   d   e   f   g   h"))
      
;; Convert the board data to a string
(defun display-board (board)
    (if (and (= (length board) width)
             (= (apply #'+ (mapcar #'length board)) (* height width)))
        (display-correct-board board)
        (format t "~A~%~%" "Incorrect 'board' data...")))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;; The readable list of all possible moves
;;
(defun pretty-colour-plus (piece-colour)
    (if (= piece-colour white) "white" "black"))

(defun pretty-type-plus (piece-type)
    (cond   ((= piece-type King)   "King  ")
            ((= piece-type Queen)  "Queen ")
            ((= piece-type Bishop) "Bishop")
            ((= piece-type Knight) "Knight")
            ((= piece-type Rook)   "Rook  ")
            ((= piece-type Pawn)   "Pawn  ")
            (t                     " _    ")))

(defun pretty-promotion-piece ()
    (concatenate 'string 
        "Promotion piece is set to " 
        (pretty-type-plus *promotion-piece*)))

(defun set-minor-promotion ()
    (minor-promotion)
    (format t "~A" (pretty-promotion-piece)))

(defun set-major-promotion ()
    (major-promotion)
    (format t "~A" (pretty-promotion-piece)))

(defun pretty-piece-plus (piece-value)
    (let ((piece-colour (pretty-colour-plus (colour piece-value)))
          (piece-type   (pretty-type-plus (absolute-piece-value piece-value))))
            (concatenate 'string  piece-colour " " piece-type)))

(defun pretty-move (move)
    (let ((move-letter (code-char (+ 96 (first  move))))
          (move-number (code-char (+ 48 (second move)))))
            (concatenate 'string (list move-letter move-number))))

(defun location-value (board x y)
    (nth (- x 1) (nth (- y 1) board)))

(defun safe-location-value (board x y)
    (if (or (< x 1) (> x width) (< y 1) (> y height))
        outside
        (nth (- x 1) (nth (- y 1) board))))

(defun is-empty? (board x y)
    (= empty (location-value board x y)))

(defun safe-is-empty? (board x y)
    (= empty (safe-location-value board x y)))

(defun equal-piece? (base-move test-move)
    (equal (first base-move) (first test-move)))

(defun non-equal-piece? (base-move test-move)
    (not (equal-piece? base-move test-move)))

(defun eq-location? (x1 y1 x2 y2)
    (and (= x1 x2) (= y1 y2)))

;; Helper for erasing pawn after an en-passant move
(defun empty-location-y (board ex ey y)
    (do ((x width (- x 1))
         (rv NIL (cons (if (eq-location? ex ey x y)
                         empty
                         (location-value board x y)) rv)))
        ((< x 1) rv )))

(defun empty-location (board ex ey)
    (do ((y height (- y 1))
        (rv NIL (cons (empty-location-y board ex ey y) rv)))
        ((< y 1) rv)))

(defun sweep-by-piece (moves)
    (if (null moves)
        NoMoves
        (let ((from-piece (first (first moves)))
              (from-moves (remove-if-not (lambda (nm) (equal-piece?     (first moves) nm)) moves))
              (rest-moves (remove-if-not (lambda (nm) (non-equal-piece? (first moves) nm)) moves)))
            (cons
                (cons from-piece (mapcar #'second from-moves))
                (sweep-by-piece rest-moves)))))

(defun add-piece-value (board moves)
    (let ((x (first  (first moves)))
          (y (second (first moves))))
        (cons
            (cons   (location-value board x y)
                    (list (first moves)))
            (rest moves))))

(defun add-piece-values (board sweeped-moves)
    (mapcar (lambda (moves) (add-piece-value board moves)) sweeped-moves))

(defun sweep (board moves)
    (add-piece-values board (sweep-by-piece moves)))

(defun pretty-moves-plus (from-move to-moves)
    (if (null to-moves)
        ""
        (concatenate 'string  
            from-move "-" 
            (first to-moves) "  "
            (pretty-moves-plus from-move (rest to-moves)))))

;;; List for one piece
(defun pretty-list-moves (moves)
    (let ((piece-description (pretty-piece-plus (first (first moves))))
          (from-move (pretty-move (second (first moves))))
          (to-moves  (mapcar #'pretty-move (rest moves))))
        (format t "~A~A~A~%"
            piece-description " :  " 
            (pretty-moves-plus from-move to-moves))))

;;; A bit pretty print of all moves
(defun list-moves (moves)
    (mapcar #'pretty-list-moves moves)
    NoMoves)
      
;; En-passant is only valid for one move
;;
(defun column-erase-en-passant-states (piece-value)
    (if (can-take-En-Passant? piece-value)
        (get-piece-type piece-value)
        piece-value))
      
(defun row-erase-en-passant-states (row-pieces)
    (mapcar (lambda (piece) (column-erase-en-passant-states piece)) row-pieces))

(defun erase-en-passant-states (board)
    (mapcar (lambda (pieces) (row-erase-en-passant-states pieces)) board))

;; Next step is to generate a list of all possible moves on a board for the player at move
;; First - generate a list of all possible moves for a given location on the board
;; Locations on the chess board are a1..h8
;; In this code for 'a' through 'h' we use 1 through 8, so e2 is entered as (5 2)

;; A standard check for a move of King or Knight
;; Either move to an empty spot or move for a take of opponents piece 
(defun check-move-King-Knight (board x y piece-colour possible-move)
    (let ((next-piece-value
            (safe-location-value board
                (+ x (first  possible-move))
                (+ y (second possible-move)))))
        (and (not   (= next-piece-value outside))
             (or    (= next-piece-value empty)
                    (= piece-colour (- (colour next-piece-value)))))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; King moves
;;
;; Only check the standard King moves        
(defun check-king-moves (board x y piece-colour possible-moves)
    (remove-if-not (lambda (move) (check-move-King-Knight board x y piece-colour move)) possible-moves))

;; Here check the possible castling moves
(defun check-castling-king-moves (board x y piece-value)
    (if (can-do-Castling? piece-value)
        (append (if (and (is-empty? board (- x 1) y)
                         (is-empty? board (- x 2) y)
                         (is-empty? board (- x 3) y)
                         (= (piece (colour piece-value) Rook Castling)
                            (safe-location-value board (- x 4) y)))
                    '((-3 0))
                    NoMoves )
                (if (and (is-empty? board (+ x 1) y)
                         (is-empty? board (+ x 2) y)
                         (= (piece (colour piece-value) Rook Castling)
                            (safe-location-value board (+ x 3) y)))
                    '((2 0))
                    NoMoves ))
        NoMoves ))
                  
(defun king-moves (board x y piece-value)
    (let ((standard-king-moves   '( (-1 -1) (0 -1) (1 -1)
                                    (-1  0)         (1  0)
                                    (-1  1) (0  1) (1  1))))
        (append (check-castling-king-moves board x y piece-value)
                (check-king-moves board x y (colour piece-value) standard-king-moves))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Knight moves
;;
;; Check for all possible Knight moves  
(defun check-knight-moves (board x y piece-colour possible-moves)
    (remove-if-not (lambda (move) (check-move-King-Knight board x y piece-colour move)) possible-moves))

(defun knight-moves (board x y piece-value)
    (let ((all-knight-moves  '( (-1 -2) (-1  2) (-2 -1) (-2 1)
                                ( 1 -2) ( 1  2) ( 2 -1) ( 2 1))))
        (check-knight-moves board x y (colour piece-value) all-knight-moves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Pawn moves
;;
;; Check for all possible Pawn moves 
(defun reverse-direction (move)
    (mapcar (lambda (x) (negate x)) move))

(defun direct-moves (moves direction)
    (if (= direction 1)
        moves
        (mapcar (lambda (move) (reverse-direction move)) moves)))

(defun check-pawn-move (board x y move)
    (let ((move-piece-value
            (safe-location-value board
                (+ x (first  move))
                (+ y (second move)))))
        (and (not (= move-piece-value outside))
             (= move-piece-value empty))))

;; Standard take check
(defun check-pawn-take (board x y piece-colour take)
    (let ((take-piece-value
            (safe-location-value board
                (+ x (first  take))
                (+ y (second take)))))
        (and (not (= take-piece-value outside))
             (= piece-colour (- (colour take-piece-value))))))

;; En-passant check
(defun check-pawn-en-passant (board x y piece-colour take)
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
            NIL)))

(defun check-pawn-moves (board x y step-moves take-moves piece-colour direction first-move)
    (let ((moves (direct-moves
            (if first-move
                step-moves
                (list (first step-moves))) direction))
          (takes (direct-moves take-moves direction)))
        (append (remove-if-not (lambda (move) (check-pawn-move board x y move)) moves)
                (remove-if-not (lambda (take) (check-pawn-en-passant board x y piece-colour take)) takes)
                (remove-if-not (lambda (take) (check-pawn-take board x y piece-colour take)) takes))))

(defun pawn-moves (board x y piece-value)
    (let* ((step-moves  '((0 1) (0 2)))
           (take-moves  '((-1 1) (1 1)))
           (piece-colour (colour piece-value))
           (direction   (if (= (colour piece-value) white) 1 -1))
           (first-move
                (or (and (= y 2) (= piece-colour white) (is-empty? board x 3))
                    (and (= y 7) (= piece-colour black) (is-empty? board x 6)))))
        (check-pawn-moves board x y step-moves take-moves piece-colour direction first-move)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; helper for Queen, Bishop and Rook moves
;;
(defun one-moves-list (moves)
    (apply #'append (mapcar (lambda (step) (safe-nth step moves)) '(0 1 2 3 4 5 6 7))))

(defun walk-directional-step (board x y dx dy step piece-colour)
    (let ((px (+ x (* dx step)))
          (py (+ y (* dy step))))
        (append
            (cond   ((= (safe-location-value board px py) outside) NoMoves)
                    ((safe-is-empty? board px py)
                        (cons (list (* dx step) (* dy step))
                            (walk-directional-step board x y dx dy (+ step 1) piece-colour)))
                    ((not (= piece-colour (colour (safe-location-value board px py))))
                        (list (list (* dx step) (* dy step))))
                    (t NoMoves)))))

(defun moves-per-direction (board x y piece-colour direction)
    (let ((dx (first  direction))
          (dy (second direction)))
        (walk-directional-step board x y dx dy 1 piece-colour)))

(defun moves-for-all-directions (board x y piece-colour directions)
    (one-moves-list
    (mapcar
        (lambda (direction) (moves-per-direction board x y piece-colour direction))
            directions)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Queen moves
;;
(defun queen-moves (board x y piece-value)
    (let ((directions '((-1 -1) (0 -1) (1 -1)
                        (-1  0)        (1  0)
                        (-1  1) (0  1) (1  1)))
         (piece-colour (colour piece-value)))
            (moves-for-all-directions board x y piece-colour directions)))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Bishop moves
;;
(defun bishop-moves (board x y piece-value)
    (let ((directions '((-1 -1) (1 -1) (-1  1) (1  1)))
          (piece-colour (colour piece-value)))
            (moves-for-all-directions board x y piece-colour directions )))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Rook moves
;;
(defun rook-moves (board x y piece-value)
    (let ((directions '((0 -1) (-1  0) (1  0) (0  1)))
          (piece-colour (colour piece-value)))
            (moves-for-all-directions board x y piece-colour directions )))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for an arbitrary piece on a arbitrary position
;;
(defun moves-for-location (board x y)
    (if (or (< x 1) (> x width) (< y 1) (> y height))
        NoMoves
        (let* ((piece-value (location-value board x y))
               (apv (absolute-piece-value piece-value)))
                (remove nil
                    (cond   ((= apv King)   (king-moves   board x y piece-value))
                            ((= apv Queen)  (queen-moves  board x y piece-value))
                            ((= apv Bishop) (bishop-moves board x y piece-value))
                            ((= apv Knight) (knight-moves board x y piece-value))
                            ((= apv Rook)   (rook-moves   board x y piece-value))
                            ((= apv Pawn)   (pawn-moves   board x y piece-value))
                            (t              NoMoves))))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for all pieces by colour
;;
;; example: ((3 (2 1)) (1 3) (3 3))
;; Piece value 3 is Knight  -  at (2 1) is b1  -  can move to (1 3) is a3 or (3 3) is c3
;;
(defun translate (x y moves)
    (if (null moves)
        NoMoves
        (let ((move (first moves)))
            (cons   (list (+ x (first move)) (+ y (second move)))
                    (translate x y (rest moves))))))

(defun all-moves-x-y (board piece-colour x y)
    (let ((piece-value (safe-location-value board x y)))
        (if (or (= piece-value outside)
                (= piece-value empty)
                (not (= piece-colour (colour piece-value))))
            NoMoves
            (let ((next-moves (moves-for-location board x y)))          
                (if (null next-moves)
                    NoMoves
                    (cons (list piece-value (list x y))
                          (translate x y next-moves)))))))

(defun all-moves-y (board piece-colour y)
    (do ((x width (- x 1))
         (rv nil (cons (all-moves-x-y board piece-colour x y) rv)))
        ((< x 1) rv)))

(defun all-moves-loop (board piece-colour)
    (do ((y height (- y 1))
         (rv nil (append (all-moves-y board piece-colour y) rv)))
         ((< y 1) rv )))

;; This is the function without the King-check test 
(defun all-moves-nct (board piece-colour)   
    (remove nil (all-moves-loop board piece-colour)))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Check if a board position is 'Check'
;;
(defun get-position-y (board player-piece y)
    (do ((x width (- x 1))
         (rv NIL (cons (if (= player-piece (get-piece-type (location-value board x y)))
                            (list x y)
                            NIL) rv)))
        ((< x 1) rv )))

(defun get-position (board player-piece)
    (first (remove-if-not (lambda (ppos) (not (null ppos)))
        (apply #'append 
            (do ((y height (- y 1))
                   (rv NIL (cons (get-position-y board player-piece y) rv)))
            ((< y 1) rv))))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Misc. function for making all the moves on the board
;;

;; Helper for changing the state of a piece (like remove the castling option)
(defun fill-location-y (board piece-value px py y)
    (do ((x width (- x 1))
         (rv NIL (cons (if (eq-location? px py x y)
                            piece-value
                            (location-value board x y)) rv)))
        ((< x 1) rv )))

(defun  fill-location (board piece-value px py)
    (do ((y height (- y 1))
        (rv NIL (cons (fill-location-y board piece-value px py y) rv)))
        ((< y 1) rv)))

;; Erase the 'from' location and fill the 'to' location
(defun work-move-y (board piece-value from-x from-y to-x to-y y)
    (do ((x width (- x 1))
        (rv NIL (cons (cond ((eq-location? from-x from-y x y) empty)
                            ((eq-location? to-x   to-y   x y) piece-value)
                            (t                                (location-value board x y)))
            rv )))
        ((< x 1) rv )))

(defun  work-move (board piece-value from-x from-y to-x to-y)
    (do ((y height (- y 1))
        (rv NIL (cons (work-move-y board piece-value from-x from-y to-x to-y y) rv)))
        ((< y 1) rv)))

(defun work-next-standard-move (board from-x from-y to-x to-y)
    (let ((piece-type (get-piece-type (location-value board from-x from-y))))
        (work-move board piece-type from-x from-y to-x to-y)))

(defun set-En-Passant (piece-type)
    (if (< piece-type 0)
        (- piece-type En-Passant)
        (+ piece-type En-Passant)))

(defun work-two-step-pawn-move (board from-x from-y to-x to-y)
    (let ((piece-type (get-piece-type (location-value board from-x from-y))))
        (work-move board (set-En-Passant piece-type) from-x from-y to-x to-y)))

;; Work the En-Passant move!
(defun work-pawn-en-passant-take (board from-x from-y to-x to-y)
    (let ((next-board (work-next-standard-move board from-x from-y to-x to-y)))
        (empty-location next-board to-x from-y)))

;; Work the Promotion of the pawn
;; Ask for a major or minor promotion
(defun work-promotion-move (board from-x from-y to-x to-y)
    (let ((piece-colour (colour (location-value board from-x from-y))))
        (work-move board (* piece-colour *promotion-piece*) from-x from-y to-x to-y)))

(defun work-next-pawn-move (board from-x from-y to-x to-y)
    ;; Test for promotion
    (if (or (= 1 to-y) (= height to-y))
        (work-promotion-move board from-x from-y to-x to-y)
        (if (and (= (abs (- from-x to-x)) 1) (is-empty? board to-x to-y))
            (work-pawn-en-passant-take board from-x from-y to-x to-y)
            (if (= (abs (- from-y to-y)) 1)
                (work-next-standard-move board from-x from-y to-x to-y)
                (work-two-step-pawn-move board from-x from-y to-x to-y)))))

;; Reset the Castling state for a piece with a given colour
(defun erase-can-castling-state (board x y)
    (fill-location board (get-piece-type (location-value board x y)) x y))

(defun work-castling-move (board from-x from-y to-x to-y)
  (let ((board-1 (work-next-standard-move board from-x from-y to-x to-y)))
    (if (= to-x 7)
        (work-next-standard-move board-1 height from-y 6 to-y)
        (work-next-standard-move board-1 1      from-y 3 to-y))))
  
(defun work-next-king-move (board from-x from-y to-x to-y)
    (if (> (abs (- from-x to-x)) 1)
        (work-castling-move board from-x from-y to-x to-y)
        (work-next-standard-move board from-x from-y to-x to-y)))
      
;; Move to piece
;; Change the state of a Pawn after its first move
;; Change the status of a Rook and King of their colour when any of the three have made a move
;;
(defun work-next-move (board piece-type from-x from-y to-x to-y)
    (cond   ((= piece-type Pawn) (work-next-pawn-move board from-x from-y to-x to-y))
            ((= piece-type King) (work-next-king-move board from-x from-y to-x to-y))
            (t                   (work-next-standard-move board from-x from-y to-x to-y))))

(defun current-piece-data (board x y)
  (let ((piece-value (location-value board x y)))
    (list (get-piece-type piece-value) (get-piece-state piece-value))))

;; Move is checked for legality
;; 
(defun make-move (board move)
    (if (null move)
        board
        (let*  ((from-x  (first  (first  move)))
                (from-y  (second (first  move)))
                (to-x    (first  (second move)))
                (to-y    (second (second move)))
                (piece-data   (current-piece-data board from-x from-y))
                (piece-type   (abs (first piece-data)))
                (piece-state  (second piece-data))
                (next-board-1 (erase-en-passant-states board))
                (next-board-2
                    (if (eql piece-state Castling)
                        (if (or (eql piece-type King) (eql piece-type Rook))
                            (erase-can-castling-state next-board-1 from-x from-y)
                            next-board-1)
                        next-board-1)))
            (work-next-move next-board-2 piece-type from-x from-y to-x to-y))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Check if a board position is a 'Checkmate'
;;
;; Assumption that the board position is already a 'check' situation
;; This to 'save' time. But it will always work correct.
;;
(defun is-checked-by? (board player-colour)
    (let ((to-moves (mapcar #'second (all-moves-list-check-test board player-colour)))
          (opponents-king-position (get-position board (* (opponents-colour player-colour) King))))
        (member opponents-king-position to-moves :test #'equal)))

;;  (is-checked-by? cmb white)

(defun single-check-test? (board player-colour opponents-rescue-moves)
    (if (null opponents-rescue-moves)
        T
        (let ((rescue-board (make-move board (first opponents-rescue-moves))))
            (if (is-checked-by? rescue-board player-colour)
                (single-check-test? board player-colour (rest opponents-rescue-moves))
                NIL))))

;; Test all opponents legal moves to see if the 'Check' can be rescued
;;
;; Also test for a draw.
;;
(defun is-checkmate-by? (board player-colour)
    (let ((rescue-moves (all-moves-list-check-test board (opponents-colour player-colour))))
        (and (is-checked-by? board player-colour)
             (single-check-test? board player-colour rescue-moves))))

(defun is-stalemate? (board player-colour)
    (let ((pieces (length (remove-if-not (lambda (piece) (not (= piece empty))) (apply #'append board)))))
        (or (= pieces 2) ;; only two kings left
            (null (all-moves board player-colour)))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Create a single list of all moves (from -> to) without piece value
;;
(defun strip-piece-value (moves)
    (let ((from (second (first moves)))
          (to   (rest moves)))
        (mapcar (lambda (to-move) (list from to-move)) to)))


(defun player-King-checked? (board move piece-colour)
    (let ((test-board (make-move board move)))
        (is-checked-by? test-board (opponents-colour piece-colour))))

(defun remove-King-checked? (board move piece-colour)
    (not (player-King-checked? board move piece-colour)))

(defun all-moves-list (board piece-colour)
    (let ((all-possible-moves
;;            (mapcan #'identity (append 
            (apply #'append
                (mapcar (lambda (moves) (strip-piece-value moves)) (all-moves-nct board piece-colour)))))
        (remove-if-not (lambda (move) (remove-King-checked? board move piece-colour)) all-possible-moves)))

;; Seperate function for 'Check' and 'Checkmate' testing
;; This is to prevent an endless recursive loop
(defun all-moves-list-check-test (board piece-colour)
    (apply #'append (mapcar (lambda (moves) (strip-piece-value moves)) (all-moves-nct board piece-colour))))

;; Here the list is first created via 'all-moves-1'
;; Next reshaped and checked for King-check-moves
;; As a last step reshaped the the 'moves-per-piece' list
(defun all-moves (board player-colour)
    (sweep board (all-moves-list board player-colour)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Evaluate and score a board position for a given player
;;
(defun get-piece-type-value (abs-piece-type pawn-factor)
    (cond   ((= abs-piece-type Queen)  Queen-value)
            ((= abs-piece-type Bishop) Bishop-value)
            ((= abs-piece-type Knight) Knight-value)
            ((= abs-piece-type Rook)   Rook-value)
            ((= abs-piece-type Pawn)   (* pawn-factor Pawn-value))
            (t                         0)))

(defun get-piece-value-x-y (board player-colour x y)
    (let ((piece-type  (location-value board x y))
          (pawn-factor (if  (= player-colour white)
                            y
                            (- 9 y))))
        (if (or (= piece-type empty) (= (opponents-colour player-colour) (colour piece-type)))
            0
            (get-piece-type-value (absolute-piece-value piece-type) pawn-factor))))
        
(defun get-pieces-value-y (board player-colour y)
    (do ((x width (- x 1))
        (rv NIL (cons (get-piece-value-x-y board player-colour x y) rv)))
        ((< x 1) rv )))

(defun get-pieces-value (board player-colour)
    (do ((y height (- y 1))
        (rv NIL (append (get-pieces-value-y board player-colour y) rv)))
        ((< y 1) rv)))

;; --- --- Knight bonusses code --- --- --- --- --- --- --- --- ---
;;
;; No range checking...
(defun knight-bonus (kx ky player-colour)
    (if (= player-colour white)
        (nth (- kx 1) (nth (- ky 1) Knight-position-bonus-white) )
        (nth (- kx 1) (nth (- ky 1) Knight-position-bonus-black) )))

(defun knight-bonus-xy (board player-colour kx ky)
    (let ((piece-type (location-value board kx ky)))
        (if (= piece-type (* player-colour Knight))
            (knight-bonus kx ky player-colour)
            0)))
    
(defun knight-bonusses-y (board player-colour y)
    (do ((x width (- x 1))
        (rv NIL (cons (knight-bonus-xy board player-colour x y) rv)))
        ((< x 1) rv )))

(defun knight-bonusses (board player-colour)
    (apply #'+ 
        (do ((y height (- y 1))
            (rv NIL (append (knight-bonusses-y board player-colour y) rv )))
            ((< y 1) rv))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; My evaluate function - and yes, I'm not a good chess player

(defun evaluate (board player-colour)
  (let ((player-pieces-values    (get-pieces-value board player-colour))
        (opponents-pieces-values (get-pieces-value board (opponents-colour player-colour)))
        (sum-knight-bonus        (-  (knight-bonusses board player-colour)
                                     (knight-bonusses board (opponents-colour player-colour))))
        (checked-by-player       (is-checked-by? board player-colour))
        (checked-by-opponent     (is-checked-by? board (opponents-colour player-colour))))
    (apply #'+ (list 
                    (apply #'+ player-pieces-values)
                    (negate (apply #'+ opponents-pieces-values))
                    sum-knight-bonus
                    (if checked-by-player
                        (if (is-checkmate-by? board player-colour)
                            Checkmate-value
                            Check-value)
                        0)
                    (if checked-by-opponent
                        (if (is-checkmate-by? board (opponents-colour player-colour))
                            (negate Checkmate-value)
                            (negate Check-value))
                        0)))))

(defun display-evaluation-score (board player-colour)
    (format t "~%current board value:~%~A" (evaluate board player-colour)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Here the code for a computer / Lisp-code generated legal move
;; Code will first check the 'opening library' - and be aware - there is NO check for legal moves on that library
;;
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; (deep) tree search - min-max only   -   no alpha-beta pruning
;;
(defun max-scores (scores)
    (let ((max-score (reduce #'max (mapcar #'first scores))))
        (remove-if-not (lambda (score) (= (first score) max-score)) scores)))

(defun search-tree (move board player-colour depth)
    (if (null move)
        Stalemate-value ;; Either this or the negative value ???
        (let* ((next-board (make-move board move))
               (board-score (evaluate next-board player-colour)))
            (if (> board-score Checkmate-range)
                (* depth Checkmate-value)
                (if (= depth 1)
                    board-score
                    (let ((next-opponents-moves (all-moves-list next-board (opponents-colour player-colour))))
                        (if (null next-opponents-moves)
                            (- Stalemate-value)
                            (- (reduce #'max
                                        (mapcar (lambda (move)
                                            (search-tree    move
                                                            next-board
                                                            (opponents-colour player-colour)
                                                            (- depth 1))) 
                                                next-opponents-moves ))))))))))

;; A quick check if there is a mate-in-1 move
;; So there will be no useless deep tree-search for other moves
;;

(defun get-mate-in-1 (board moves player-colour)
    (let* ((next-boards   (mapcar #'(lambda (move) (make-move board move)) moves))
           (scored-boards (mapcar #'(lambda (next-board) (evaluate next-board player-colour)) next-boards))
           (scored-moves  (mapcar #'list scored-boards moves))
           (rls           (max-scores scored-moves)))
        (if (null rls)
            NoMoves
            (if (> (first (first rls)) Checkmate-range)
                (second (first rls))
                NoMoves))))

;; Pick a random element from a list
(defun random-element (ls)
    (nth (random (length ls)) ls))

(defun best-move (board player-colour)
    (let* ((next-player-moves (all-moves-list board player-colour))
           (mate-in-1 (get-mate-in-1 board next-player-moves player-colour)))
        (if (null mate-in-1)
            (let* ((scored-moves
                    (mapcar (lambda (move)
                        (list (search-tree move board player-colour search-depth) move))
                            next-player-moves)))
                (second (random-element (max-scores scored-moves))))
            mate-in-1)))

;; Look up board position in opening library
(defun get-opening-move (game library-game)
    (let ((gln (length game))
          (lln (length library-game)))
        (if (< gln lln)
            (if (equal game (subseq library-game 0 gln))
                (first (nthcdr gln library-game))
                NIL)
            NIL)))

(defun get-opening-library-moves (game library)
  (remove-if-not 
    (lambda (ls) (not (null ls)))
        (mapcar (lambda (library-game) (get-opening-move game library-game)) library)))

(defun find-best-move (game board player-colour open-library?)
  (if   (and (null game) open-library?)
        (list (list 5 2) (list 5 4))            ;; white will always open with "e2-e4" with a new game
        (if open-library?                       ;; in case of a 'mate-in-2' like puzzle... 
            (if (= player-colour white)
                (let ((open-moves (get-opening-library-moves (reverse game) opening-library-white)))
                    (if (null open-moves)
                        (best-move board player-colour)
                        (random-element open-moves)))
                (let ((open-moves (get-opening-library-moves (reverse game) opening-library-black)))
                    (if (null open-moves)
                        (best-move board player-colour)
                        (random-element open-moves))))
            (best-move board player-colour))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Display the list of moves made in the game
;;
(defun pretty-from-to (move)
    (format NIL "~A~A~A" 
        (pretty-move (first move))
        "-"
        (pretty-move (second move))))
  
(defun pretty-game (game)
  (cond ((null game)            (format nil "~%"))
        ((= (length game) 1)    (concatenate 'string (pretty-from-to (first game)) (format nil "~%")))
        (t                      (concatenate 'string 
                                    (pretty-from-to (first game)) "   "
                                    (pretty-from-to (second game)) (format nil "~%")
                                    (pretty-game    (rest (rest game)))))))

(defun display-game (rgame)
  (let ((game (reverse rgame)))
    (unless (null game)
      (format t "~%The played game:~%~A" (pretty-game game)))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; FEN conversions
;; First: open library format to FEN
;;
;; Change the '.' for empty fields to the number of consecutive empty fields
;;
;; First some FEN helpers - old openings library incl. helpers
;;
(defun get-piece-character (piece-type)
    (cond   ((= piece-type King)   #\K)
            ((= piece-type Queen)  #\Q)
            ((= piece-type Bishop) #\B)
            ((= piece-type Knight) #\N)
            ((= piece-type Rook)   #\R)
            ((= piece-type Pawn)   #\P)
            (t                     #\.)))

(defun convert-piece-for-openings-library (piece-value)
    (let ((ptc (get-piece-character (absolute-piece-value piece-value))))
        (string
            (if (equal (colour piece-value) black)
                (char-downcase ptc)
                (char-upcase   ptc)))))
         
(defun opening-library-style-row (board-row)
    (format nil "~{~A~}" 
        (mapcar #'convert-piece-for-openings-library board-row)))         

(defun opening-library-style (board)
    (format nil "~{~A~}" 
        (reverse (mapcar #'opening-library-style-row board))))

(defun count-empties-seq (rps)
    (if (null rps)
        0
        (if (equal (first rps) #\.)
            (+ 1 (count-empties-seq (rest rps)))
            0)))

(defun count-empties (rps)
  (code-char (+ (count-empties-seq rps) 48)))

(defun skip-empties (rps)
    (if (null rps)
        NIL
        (if (equal (first rps) #\.)
            (skip-empties (rest rps))
            rps)))
  
(defun to-FEN-style-row (rps)
    (if (null rps)
        NIL
        (let ((f (first rps)))
            (if (equal f #\.)
                (cons (count-empties rps) (to-FEN-style-row (skip-empties rps)))
                (cons f (to-FEN-style-row (rest rps)))))))

(defun to-FEN-style (pls)
    (if (null pls)
        NIL
        (cons
            (cons #\/ (to-FEN-style-row (subseq pls 0 width)))  ;; take
                      (to-FEN-style     (nthcdr width pls)))))  ;; drop

;; Drop the first character of a string - safe
(defun string-rest (s)
    (if (string= s "")
        ""
        (subseq s 1)))

(defun ols-to-FEN-style (ols player-colour)
    (let ((pls (coerce ols 'list))) ; string->list equivalent
        (format nil "~A ~A - -"
            (string-rest
                (apply #'concatenate 'string 
                    (mapcar (lambda (row) (coerce row 'string)) 
                            (to-FEN-style pls))))
            (if (equal player-colour white) "w" "b"))))

(defun display-FEN (board player-colour)
    (let ((ols (opening-library-style board)))
        (format t "~%~A~%" (ols-to-FEN-style ols player-colour))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Read commands or moves from the keyboard
;;
;; First the simple parse only for format 'e2e4'
;; (for now 'e2e4' is the working format)
;;
;; Commands:
;;  b     show the current board
;;  c     play a Lisp-code-generated move
;;  e     show the evaluation score of the current board position
;;  f     show the FEN string of the current board position
;;  g     show all the previous moves
;;  h     show this helper information
;;  m     show all possible moves for the current player
;;  M     show all possible moves for the opponent
;;  p     set promotion piece to minor promotion (Knight) before the actual move
;;  P     set promotion piece to major promotion (Queen) before the actual move
;;  s     show the current promotion piece
;;  q     quit the game
;;
(defun display-helper-information ()
    (code-info)
    (format t "~A~%~%"  "helper information")
    (format t "~A~%~%"  "  the input format for a move (like): e2e4")
    (format t "~A~%"    "  commands:")
    (format t "~A~%"    "    b     show the current board")
    (format t "~A~%"    "    c     play a Lisp-code-generated move")
    (format t "~A~%"    "    e     show the evaluation score of the current board position")
    (format t "~A~%"    "    f     show the FEN string of the current board position")
    (format t "~A~%"    "    g     show all the previous moves")
    (format t "~A~%"    "    h     show this helper information")
    (format t "~A~%"    "    m     show all possible moves for the current player")
    (format t "~A~%"    "    M     show all possible moves for the opponent")
    (format t "~A~%"    "    p     set promotion piece to minor promotion (Knight) before the actual move")
    (format t "~A~%"    "    P     set promotion piece to major promotion (Queen) before the actual move")
    (format t "~A~%"    "    s     show the current promotion piece")
    (format t "~A~%~%"  "    q     quit the game."))

(defun parse-move (board player-colour entered-move game open-library?)
    (let ((move (coerce entered-move 'list)))
        (if (null move)
            NoMoves
            (if (= 4 (length move))
                (move-to-list entered-move)     ;; in 'opening-library.lisp'
                (let ((cmd (first move)))
                    (cond   ((equal cmd #\b) (display-board board))
                            ((equal cmd #\c) (find-best-move game board player-colour open-library?))
                            ((equal cmd #\e) (display-evaluation-score board player-colour))
                            ((equal cmd #\f) (display-FEN board player-colour))
                            ((equal cmd #\g) (display-game game))
                            ((equal cmd #\h) (display-helper-information))
                            ((equal cmd #\m) (list-moves (all-moves board player-colour)))
                            ((equal cmd #\M) (list-moves (all-moves board (negate player-colour))))
                            ((equal cmd #\p) (set-minor-promotion))
                            ((equal cmd #\P) (set-major-promotion))
                            ((equal cmd #\s) (format t "~A" (pretty-promotion-piece)))
                            ((equal cmd #\q) QuitGame)                    
                            (t               NoMoves)))))))

;; Read, parse and check for legality of the move
;; A legal move will return that move
;; otherwise it will return an empty list if the move is illegal
;;
(defun next-move (board player-colour game open-library?)
    (format t   "~%~c[102m~c[30m~c[1mNext move for player ~A:~c[0m " 
                #\ESC #\ESC #\ESC (pretty-colour-plus player-colour) #\ESC )
    (force-output)  ;; Ensure the prompt is visible before reading
    (let* ((input-move (read-line))
           (move (parse-move board player-colour input-move game open-library?))
           (legal-moves (all-moves-list board player-colour)))
        (cond   ((or (null move) (eq move :void)) ;; CL uses null for empty lists
                    (format t ""))
                ((< (first (first move)) 9)
                    (format t "~%move: ~A" (pretty-from-to move)))
                (t  (format t "")))
        (if (or (eql move QuitGame) (member move legal-moves :test #'equal))
            move
            NoMoves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Minimal game loop
;;
(defun get-move (board player-colour game open-library?)
    (let ((move (next-move board player-colour game open-library?)))
        (if (null move)
            (get-move board player-colour game open-library?)
            move)))

(defun show-check (board player-colour)
    (if (is-checked-by? board player-colour)
        (format t "* Check!  by  ~A~%~%" (pretty-colour-plus player-colour))
        (format NIL "")))

;; Function to show to board with the checkmate position
(defun checkmate-board (board player-colour game)
    (display-board board)
    (cons (* player-colour Checkmate) game))

;; Function to show to board with the a draw position
(defun draw-board (board player-colour game)
    (display-board board)
    (cons (* player-colour Draw) game))

(defun game-loop (board player-colour game open-library?)
    (display-board board)
    (show-check board (opponents-colour player-colour))
    (let ((move (get-move board player-colour game open-library?)))
        (if (equal move QuitGame)
            (cons Quit. game)
            (let ((next-board (make-move board move))
                  (next-game  (cons move game)))
                (if (is-checkmate-by? next-board player-colour)
                    (checkmate-board next-board player-colour next-game)
                    (if (is-stalemate? next-board (opponents-colour player-colour))
                        (draw-board next-board player-colour next-game)
                        (game-loop next-board (opponents-colour player-colour) next-game open-library?)))))))

;; Start the game-loop en show end status
;; Incl. Quit or Checkmate test
(defun play-chess (board player-colour)
    (let ((open-library? (and (equal board initial-board)
                              (= player-colour white))))
        (code-info)
        (format t "~%~A~%~%" "Have a good game!")
        (let* ((game (game-loop board player-colour NoMoves open-library?))
               (game-state (abs (first game))))
            (display-game (rest game))
            (cond   ((= game-state Checkmate)
                        (format t "~%* * *  Checkmate! * * *   by  ~A ~%~%Thank you and bye bye.~%~%"
                            (pretty-colour-plus (colour (first game)))))
                    ((= game-state Draw)
                        (format t "~%* a draw *~%~%~A~%~%" "Thank you and bye bye."))
                    (t  (format t "~%~A~%~%" "You quit. Thank you and bye bye."))))))


;;
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Testing & debugging . . .
;;


;; For the run from terminal
;; (defun main ()  (play-chess initial-board      white))
;; 
(defun main ()  (play-chess Mate-in-4-white-01 white))   ;; speed check

(defun t1 ()    (play-chess initial-board      white))
(defun m2w1 ()  (play-chess Mate-in-2-white-01 white))
(defun m2w2 ()  (play-chess Mate-in-2-white-02 white))
(defun m2w3 ()  (play-chess Mate-in-2-white-03 white))
(defun m2w4 ()  (play-chess Mate-in-2-white-04 white))
(defun m2w5 ()  (play-chess Mate-in-2-white-05 white))
(defun m2w6 ()  (play-chess Mate-in-2-white-06 white))
(defun m2w7 ()  (play-chess Mate-in-2-white-07 white))
(defun m2b1 ()  (play-chess Mate-in-2-black-01 black)) ;; black
(defun m4w1 ()  (play-chess Mate-in-4-white-01 white))
(defun mNw1 ()  (play-chess Mate-in-N-white-01 white))

;;(main)
;;(t1)

;; Mate-in-2 serie
;;(m2w1)
;;(m2w2)
;;(m2w3)
;;(m2w4)
;;(m2w5)
;;(m2w6)
;;(m2w7)
;;(m2b1)

;;(m4w1)
;;(mNw1)
;;(pD)
;;(tM)

#| 
(sb-ext:save-lisp-and-die "qChess"
    :toplevel #'main
    :executable t
    :save-runtime-options t     ; optional: keeps *standard-input* etc working nicely
    ;; :compression 9           ; uncomment if your SBCL supports core compression
    )
|#
;; sbcl --non-interactive --load chess.lisp

;;
;; End of code.
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

