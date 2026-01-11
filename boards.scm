;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "boards.scm"
;;
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.02a   2026-01-10    A version with two boards, incl. 'initial-board'
;;  version 0.02b   2026-01-11    Adding 'Mate in 2' boards
;; 
;;  (cl) 2025-12-31, 2026-01-11 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; Using the 'scheme' language in DrRacket
;; 
#lang scheme

(require "chess-pieces.scm")

(provide initial-board)
(provide test-board)
(provide Mate-in-2-white-01)
(provide Mate-in-2-white-02)


(define initial-board
  (list
   (list (piece white Rook Castling) (piece white Knight) (piece white Bishop) (piece white Queen)
         (piece white King Castling) (piece white Bishop) (piece white Knight) (piece white Rook Castling))
   (list (piece white Pawn First-Move) (piece white Pawn First-Move)
         (piece white Pawn First-Move) (piece white Pawn First-Move)
         (piece white Pawn First-Move) (piece white Pawn First-Move)
         (piece white Pawn First-Move) (piece white Pawn First-Move))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece black Pawn First-Move) (piece black Pawn First-Move)
         (piece black Pawn First-Move) (piece black Pawn First-Move)
         (piece black Pawn First-Move) (piece black Pawn First-Move)
         (piece black Pawn First-Move) (piece black Pawn First-Move))
   (list (piece black Rook Castling) (piece black Knight) (piece black Bishop) (piece black Queen)
         (piece black King Castling) (piece black Bishop) (piece black Knight) (piece black Rook Castling))))


(define test-board
  (list
   (list (piece white Rook Castling) (piece white Knight) (piece white Bishop) (piece white Queen)
         (piece white King Castling) (piece white Bishop) (piece white Knight) (piece white Rook Castling))
   (list (piece white Pawn First-Move) (piece white Pawn First-Move)
         (piece white Pawn First-Move) (piece white Pawn First-Move)
         (piece white Pawn First-Move) (piece white Pawn First-Move)
         (piece white Pawn First-Move) (piece white Pawn First-Move))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece black Pawn First-Move) (piece black Pawn First-Move)
         (piece black Pawn First-Move) (piece black Pawn First-Move)
         (piece black Pawn First-Move) (piece black Pawn First-Move)
         (piece black Pawn First-Move) (piece black Pawn First-Move))
   (list (piece black Rook Castling) (piece black Knight) (piece black Bishop) (piece black Queen)
         (piece black King Castling) (piece black Bishop) (piece black Knight) (piece black Rook Castling))))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Mate in 2 - white to play
;;
(define Mate-in-2-white-01
  (list
   (list (piece black Queen) empty empty empty empty (piece white Rook) empty empty)
   (list (piece white Rook) (piece white Pawn First-Move) empty (piece white Pawn First-Move)
         empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty (piece white King) empty (piece white Queen) empty empty)
   (list empty empty empty empty empty empty empty (piece black King))))

;; Mate in 2 - white to play
(define Mate-in-2-white-02
  (list
   (list (piece white King) empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty (piece black Knight) empty empty empty empty)
   (list (piece white Rook) empty empty empty empty empty empty (piece white Rook))
   (list empty empty empty (piece black King) empty empty empty empty)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Empty helper board - for endgame setup
(define empty-board
  (list
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)))


;; End of this code
