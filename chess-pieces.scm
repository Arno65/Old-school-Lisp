;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-pieces.scm"
;;
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.02a   2026-01-10    From a first draft of 'chess.scm', pieces data
;;  version 0.02b   2026-01-12    Added Piece & state values for the 'evaluate' function
;; 
;;  (cl) 2025-12-31, 2026-01-12 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; Using the 'scheme' language in DrRacket
;; 
#lang scheme

(provide piece) ;; The only function here 

(provide white)
(provide black)

(provide King)
(provide Queen)
(provide Bishop)
(provide Knight)
(provide Rook)
(provide Pawn)
(provide empty)
(provide outside)

(provide Checkmate-value)
(provide Check-value)
(provide Queen-value)
(provide Bishop-value)
(provide Knight-value)
(provide Rook-value)
(provide Pawn-value)

(provide First-Move)
(provide En-Passant)
(provide Castling)


;;; -- Some definitions --
(define white    1)
(define black   -1)

(define King     6)
(define Queen    5)
(define Bishop   4)
(define Knight   3)
(define Rook     2)
(define Pawn     1)
(define empty    0)
(define outside -9) ;; Outside of the board

;; Piece and state values
(define Checkmate-value 999999)
(define Check-value       1000)
(define Queen-value        100)
(define Bishop-value        10)
(define Knight-value        10)
(define Rook-value          10)
(define Pawn-value           1)

;; Piece states
(define First-Move 10)
(define En-Passant 20)
(define Castling   30)

;; A helper function for creating a piece
;; Either 2 or 3 parameters
(define (piece . characteristics)
  (if (= 2 (length characteristics))
      (* (first characteristics) (second characteristics))
      (if (= 3 (length characteristics))
          (* (first characteristics) (+ (second characteristics) (third characteristics)))
          empty)))

;; End of this code
