;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "opening-library.scm"
;;
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 0.02a   2026-01-10    An opening library for my chess engine
;;                                A first minimal version, working jusr fine
;;  version 0.02b   2026-01-11    Added a few extra moves
;; 
;;  (cl) 2025-12-31, 2026-01-11 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; Using the 'scheme' language in DrRacket
;; 

#lang scheme

(provide opening-library)

(define opening-library
  (list
;; e4
   (list "RNBQKBNRPPPPPPPP................................pppppppprnbqkbnr" "e2e4")
   (list "RNBQKBNRPPPPPPPP....................p...........pppp.ppprnbqkbnr" "e7e5")
   (list "RNBQKBNRPPPP.PPP............P.......p...........pppp.ppprnbqkbnr" "g1f3")
   (list "RNBQKBNRPPPP.PPP............P.......p........n..pppp.ppprnbqkb.r" "b8c6")
   (list "R.BQKBNRPPPP.PPP..N.........P.......p........n..pppp.ppprnbqkb.r" "f1b5")
   (list "R.BQKBNRPPPP.PPP..N......b..P.......p........n..pppp.ppprnbqk..r" "g8f6")
   (list "R.BQKB.RPPPP.PPP..N..N...b..P.......p........n..pppp.ppprnbqk..r" "d2d3")
   (list "R.BQKB.RPPPP.PPP..N..N...b..P.......p......p.n..ppp..ppprnbqk..r" "f8c5")
   (list "R.BQK..RPPPP.PPP..N..N...bB.P.......p......p.n..ppp..ppprnbqk..r" "c2c3")
   (list "R.BQK..RPPPP.PPP..N..N...bB.P.......p.....pp.n..pp...ppprnbqk..r" "e8g8")
   (list "R.BQ.RK.PPPP.PPP..N..N...bB.P.......p.....pp.n..pp...ppprnbqk..r" "e1g1")
   (list "R.BQ.RK.PPPP.PPP..N..N...bB.P.......p.....pp.n..pp...ppprnbq.rk." "d7d5")
   (list "R.BQ.RK.PPP..PPP..N..N...bBPP.......p.....pp.n..pp...ppprnbq.rk." "b1d2")
   (list "R.BQ.RK.PPP..PPP..N..N...bBPP.......p.....pp.n..pp.n.pppr.bq.rk." "d5e4")
   (list "R.BQ.RK.PPP..PPP..N..N...bB.P.......P.....pp.n..pp.n.pppr.bq.rk." "d3e4")

;; d4
   (list "RNBQKBNRPPPPPPPP...................p............ppp.pppprnbqkbnr" "d7d5")
   
;; c4
   (list "RNBQKBNRPPPPPPPP..................p.............pp.ppppprnbqkbnr" "e7e5")

;; Nf3
   (list "RNBQKBNRPPPPPPPP.............................n..pppppppprnbqkb.r" "g8f6")

   ))

;; End of this code
