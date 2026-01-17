;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "boards.scm"
;;
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.02a   2026-01-10    A version with two boards, incl. 'initial-board'
;;  version 0.02b   2026-01-11    Adding 'Mate in 2' boards
;;  version 0.02c   2026-01-16    Adding 'Mate in 2' boards
;;  version 0.02d   2026-01-17    Adding 'Mate in 2' boards (and a mate in N)
;; 
;;  (cl) 2025-12-31, 2026-01-17 by Arno Jacobs
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
(provide Mate-in-2-white-03)
(provide Mate-in-2-white-04)
(provide Mate-in-2-black-01)
(provide Mate-in-N-white-01)


;; FEN: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -
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
;; FEN: 5Kbk/6pp/6P1/8/8/8/8/7R w - - 
;; Stockfish:  1. Rh6 Bf7  2. Rxh7#  
(define Mate-in-2-white-01
  (list
   (list empty empty empty empty empty empty empty (piece white Rook))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty (piece white Pawn) empty)
   (list empty empty empty empty empty empty (piece black Pawn) (piece black Pawn))
   (list empty empty empty empty
         empty (piece white King) (piece black Bishop) (piece black King))))

;; Mate in 2 - white to play
;; FEN: 3k4/R6R/3n4/8/8/8/8/K7 w - -
;; Stockfish:  1. Rhg7 Nc4  2. Ra8# 
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

;; Mate in 2 - white to play
;; FEN: R1B4k/8/8/7K/8/8/p1pppppp/1Q6 w - -
;; Stockfish:  1. Kh6 cxb1=Q  2. Bb7# 
(define Mate-in-2-white-03
  (list
   (list empty (piece white Queen) empty empty empty empty empty empty)
   (list (piece black Pawn) empty (piece black Pawn) (piece black Pawn)
         (piece black Pawn) (piece black Pawn) (piece black Pawn) (piece black Pawn))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty (piece white King))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece white Rook) empty (piece white Bishop) empty empty  empty empty (piece black King))))


;; Mate in 2 - white to play
;; FEN: R5rk/6pr/1K6/8/3Q4/8/8/7R w - -
;; Stockfish:  1. Qa1 Rf8   2. Rxf8# 
(define Mate-in-2-white-04
  (list
   (list empty empty empty empty empty empty empty (piece white Rook))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty (piece white Queen) empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty (piece white King) empty empty empty empty empty empty)
   (list empty empty empty empty empty empty (piece black Pawn) (piece black Rook))
   (list (piece white Rook) empty empty empty
         empty empty (piece black Pawn) (piece black King))))



;; Mate in 2 - black to play
;; FEN: 5kBK/6rP/7n/8/8/8/8/8 b - -
;; Stockfish: 1... Rf7 2. Bxf7 Nxf7# 
(define Mate-in-2-black-01
  (list
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty
         empty empty empty (piece black Knight))
   (list empty empty empty empty empty
         empty (piece black Rook) (piece white Pawn))
   (list empty empty empty empty
         empty (piece black King) (piece white Bishop) (piece white King))))


;; Mate in # - white to play
;; FEN: 2b1k3/K7/2PP4/8/8/8/8/8 w - -
;; Stockfish:
;;  1. Ka8 Kd8    2. Kb8 Be6    3. c7+ Kd7    4. c8=Q+ Kxd6   5. Qf8+ Ke5
;;  6. Kc7 Bf5    7. Qc5+ Kf4   8. Qd4+ Be4   9. Kd6 Kf3     10. Ke5 Bg6
;; 11. Qf4+ Ke2  12. Kd4 Bh5   13. Qg3 Bf3   14. Qh2+ Kf1    15. Ke3 Bg2
;; 16. Qf4+ Kg1  17. Qg3 Kh1   18. Kf2 Bf1   19. Qg1#
;;
(define Mate-in-N-white-01
  (list
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty (piece white Pawn) (piece white Pawn)
         empty empty empty empty)
   (list (piece white King) empty empty empty empty empty empty empty)
   (list empty empty (piece black Bishop) empty 
         (piece black King) empty empty empty)))



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


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Some FEN examples
;; rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -
;; r2qkb1r/3ppppp/b1n2n2/ppp5/PPP5/B1N2N2/3PPPPP/R2QKB1R w KQkq -
;; 3k4/R6R/3n4/8/8/8/8/K7 w - -

;; End of this code
