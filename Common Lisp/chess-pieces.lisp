;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-pieces.scm"
;;
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.02a   2026-01-10    From a first draft of 'chess.scm', pieces data
;;  version 0.02b   2026-01-12    Added Piece & state values for the 'evaluate' function
;;  version 0.03a   2026-01-18    Removing the 'First-move' indicator
;;  version 0.03b   2026-01-21    Changes in piece valuation
;;  version 0.04a   2026-01-22    Changes in Knight-position-bonus, now dependent on players colour
;;  version 0.04b   2026-01-23    The common lisp version
;; 
;;  (cl) 2025-12-31, 2026-01-23 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;

;; Chess board dimensions
(defconstant width  8)
(defconstant height 8)

;;; -- Some definitions --
(defconstant white    1)
(defconstant black   -1)

(defconstant King     6)
(defconstant Queen    5)
(defconstant Bishop   4)
(defconstant Knight   3)
(defconstant Rook     2)
(defconstant Pawn     1)
(defconstant empty    0)
(defconstant outside -9) ;; Outside of the board

;; Piece and state values
(defconstant Checkmate-value 9999999)
(defconstant Checkmate-range  999999)
(defconstant Stalemate-value    9999)
(defconstant Check-value         420)
(defconstant Queen-value         900)
(defconstant Rook-value          500)
(defconstant Bishop-value        330)
(defconstant Knight-value        320)
(defconstant Pawn-value           25) ;; position dependent

;; Piece-square table
;; Knight position bonus
;;
(defconstant Knight-position-bonus-white
  (list
   (list -50 -40 -30 -30 -30 -30 -40 -50)
   (list -40 -20   0   5   5   0 -20 -40)
   (list -30   5  15  20  20  15   5 -30)  
   (list -30   0  10  15  15  10   0 -30)
   (list -30   5  15  20  20  15   5 -30)
   (list -30   0  10  15  15  10   0 -30)
   (list -40 -20   0   0   0   0 -20 -40)
   (list -50 -40 -30 -30 -30 -30 -40 -50)))

(defconstant Knight-position-bonus-black
  (reverse Knight-position-bonus-white))

;; Piece states
(defconstant En-Passant 10)
(defconstant Castling   20)

;; A helper function for creating a piece
;; Either 2 or 3 parameters
(defun piece (&rest characteristics)
    (if (= 2 (length characteristics))
        (* (first characteristics) (second characteristics))
        (if (= 3 (length characteristics))
            (*  (first characteristics) 
                (+ (second characteristics) (third characteristics)))
            empty)))

;; End of this code
