;; polyTEST.ss
;; AUCSC370
;; OCT 25, 2018
;;
;; Philippe Nadon
;; Test file for poly.ss
(display "OUTPUT OF polyTEST.ss :\n")
(load "poly.ss")

;;First poly
(display "\nFirst poly \n")
(set! poly1 (makePoly '(-5 215 4 100 -1 2 3 0)))
(writePoly poly1)

;;Second poly
(display "\nSecond poly \n")
(set! poly2 (makePoly '(2 5 6 2)))
(writePoly poly2)

;;Test zero cases
(display "\nTest zero cases \n")
(set! poly3 (makePoly '()))
(writePoly poly3)
(set! poly4 (makePoly '(0 0)))
(writePoly poly4)

;;Test 1's
(display "\nTest 1's \n")
(set! poly5 (makePoly '(1 9 1 8 1 7 1 6 1 5 1 0)))
(writePoly poly5)

;;Test monomial case & -1
(display "\nTest monomial case & -1 \n")
(set! poly6 (makePoly '(-1 0)))
(writePoly poly6)

;;Test multiplying monomial, 1's, and -1's
(display "\nTest multiplying monomial, 1's, and -1's \n")
(writePoly (multPolys poly5 poly6))

;;Test addition, combining of terms
(display "\nTest addition, combining of terms \n")
(writePoly (addPolys poly1 poly2))
(writePoly (addPolys poly1 poly1))

;;Test cases which result in coefficients of 0
(display "\nTest cases which result in coefficients of 0 \n")
(writePoly (subPolys poly1 poly1))
(writePoly (subPolys poly3 poly4))
(writePoly (diffPoly poly3))
(writePoly (multPolys poly3 poly1))
(writePoly (multPolys poly3 poly3))

;;Test other methods
(display "\nTest other methods \n")
(writePoly (multPolys poly2 poly2))
(writePoly (diffPoly poly1))

