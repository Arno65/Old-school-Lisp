;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "opening-library.scm"
;;
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.02a   2026-01-10    An opening library for my chess engine
;;                                A first minimal version, working jusr fine
;; 
;;  (cl) 2025-12-31, 2026-01-10 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; Using the 'scheme' language in DrRacket
;; 

#lang scheme

(provide opening-library)

(define opening-library
  (list
   (list "RNBQKBNRPPPPPPPP................................pppppppprnbqkbnr" "e2e4")
   (list "RNBQKBNRPPPPPPPP....................p...........pppp.ppprnbqkbnr" "e7e5")
   ))

;; End of this code
