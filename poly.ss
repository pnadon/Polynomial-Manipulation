;; poly.ss
;; AUCSC370
;; OCT 25, 2018
;;
;; Philippe Nadon
;;
;; A collection of functions for 
;; manipulating polynomials

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prints a newline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (newline)
  (display "\n")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creates formatted polynomial list from inputted list
;;
;; EX: (1 2 3 4) -> ((1 2) (3 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (makePoly userInput)
  (cond
    ((or (equal? userInput '())
    (equal? (car userInput) 0))
      '() )
    (#t (makePolyREC '() userInput)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive part of makePoly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (makePolyREC poly userInput)
  (cond
    ((< (length userInput) 2) 
      poly)
    (#t (makePolyREC 
         (append poly (list (list (car userInput) 
                                  (cadr userInput))))
         (cddr userInput))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prints out the polynomial represented in the inputted
;; list, in a nice format
;;
;; writePoly prints out the first part of the polynomial,
;; and then calls writePolyREC to print the remainder
;;
;; EX: ((1 2) (3 4)) -> x^2 + 3x^4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (writePoly poly)
  (cond
    ((null? poly) 
      (display 0) )
    (#t 
      (cond ((< (caar poly) 0) (display "-")))
      (cond
        ((or (not (equal? (abs (caar poly)) 1)) 
             (equal? (cadar poly) 0))
          (display (abs (caar poly)))))
      (cond
        ((> (cadar poly) 1)
          (display "x^")
          (display (cadar poly)))
        ((equal? (cadar poly) 1)
          (display "x")))
         
      (writePolyREC (cdr poly))))
  (newline)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive part of writePoly, prints the monomials
;; proceeding the first
;;
;; Take a polynomial list as input, and returns nothing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (writePolyREC poly)
  (cond
    ((equal? (length poly) 0) 
      (display ""))
    (#t
      (polyToString (list (caar poly) (cadar poly)))
      (writePolyREC (cdr poly))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prints a monomial as its formatted form, assuming
;; it isn't the first of the polynomial list, which needs
;; different rules for the preceding sign
;;
;; Takes a polynomial list as input, returns nothing 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (polyToString poly)
  (cond 
    ((< (car poly) 0)
      (display " - ")
      (cond
        ((or (not (equal? (car poly) -1)) 
            (equal? (cadr poly) 0))
          (display (* (car poly) -1)))))
    (#t
      (display " + ")
      (cond
        ((or (not (equal? (car poly) 1)) 
             (equal? (cadr poly) 0))
          (display (car poly))))))
  (cond
    ((> (cadr poly) 1)
      (display "x^")
      (display (cadr poly)))
    ((equal? (cadr poly) 1)
      (display "x")))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Same as addPolys, but with the second polynomial's
;; coefficients negated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (subPolys poly1 poly2)
  (addPolys poly1 (map negateMonomial poly2))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Take a member of a polynomial list (a monomial), and
;; returns the list with its coefficients negated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (negateMonomial monomial)
  (list (* -1 (car monomial)) (cadr monomial))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Takes two polynomial lists and returns the sum of the
;; two polynomials as another polynomial list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (addPolys poly1 poly2)
  (addPolysREC '() poly1 poly2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive part of addPolys
;;
;; Takes resPoly, the result of the sum of poly1 and poly2
;; Takes poly1 and poly2, which are polynomial lists
;;
;; Once finished, returns resPoly as a polynomial list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (addPolysREC resPoly poly1 poly2)
  (cond
    ((null? poly1) 
      (append resPoly poly2))
    ((null? poly2)
      (append resPoly poly1))

    (#t (cond
      ((< (cadar poly1) (cadar poly2))
        (addPolysREC 
          (append resPoly (list (car poly2)))
          poly1
          (cdr poly2)))

      ((equal? (cadar poly1) (cadar poly2))
        (cond
          ((equal? (caar poly1) (* -1 (caar poly2)))
            (addPolysREC
              resPoly
              (cdr poly1)
              (cdr poly2)))
          (#t
            (addPolysREC
              (append resPoly (list (list 
                (+ (caar poly1) (caar poly2)) (cadar poly1))))
              (cdr poly1)
              (cdr poly2)))))

      (#t
        (addPolysREC 
          (append resPoly (list (car poly1)))
          (cdr poly1)
          poly2)))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Takes two polynomials and returns their product
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (multPolys poly1 poly2)
  (cond
    ((or (null? poly1) (null? poly2))
      '() )
    (#t
      (multPolysREC '() poly1 poly2)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive portion of multPolys, Recursively multiplies
;; poly2 with every member of poly1, and returns the
;; result as resPoly, which is a polynomial list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (multPolysREC resPoly poly1 poly2)
  (cond
    ((equal? (length poly1) 0)
      resPoly)
    (#t
      (multPolysREC 
          (addPolys 
              resPoly 
              (map (lambda (monomial)
                  (list (* (caar poly1) 
                           (car monomial))
                        (+ (cadar poly1) 
                           (cadr monomial))))
                poly2))
          (cdr poly1)
          poly2)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Takes a real number xVal, and polynomial list poly, and 
;; evaluates the polynomial at x = xVal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (evalPoly xVal poly)
  (cond
    ((null? poly)  '() )
    (#t (apply +
      (map (lambda (monomial)
          (* (car monomial) (expt xVal (cadr monomial))))
        poly))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns the inputted polynomial list, differentiated 
;; with respect to its variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (diffPoly poly)
  (cond
    ((null? poly)  '() )
    (#t
      (popZeroPiece (map diffMonomial poly))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removes any  trailing 0-coefficient monomials in the
;; inputted polynomial list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (popZeroPiece poly)
  (cond
    ((equal? (caar (reverse poly)) 0)
      (reverse (cdr (reverse poly))))
    (#t poly))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Takes a monomial, and differentiates it with respect to
;; its variable, by multiplying its coefficient by its
;; exponent, and then subtracting 1 from its exponent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (diffMonomial monomial)
  (cond
    ((equal? (cadr monomial) 0)
      '(0 0))
    (#t 
      (list (* (car monomial) (cadr monomial))
    (- (cadr monomial) 1))))
)