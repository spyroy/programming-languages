#lang pl 02

#|
----------------------------------Question 1.a----------------------------------
|#

;; we define the grammer SE wich represents strings with digits only
;; <D> is a string with digits e.g "123"
;; <num> is a number e.g 123
;; <string> is a string composed from characters
;; <string-appendSE> is a sequance of strings
;; <string-insertSE> is a sequance of <SE>
;; <char> is acharacte as in racket languge e.g #\3
; λ is empty string

#|
<SE> ::= <D>| -----------------------------------------------------(1)
         <num>| ---------------------------------------------------(2)                                              
         <string>| ------------------------------------------------(3)
         <string-appendSE>| ---------------------------------------(4)
         <string-insertSE>| ---------------------------------------(5)
         <numString>| ---------------------------------------------(6)
         <string-lengthSE>-----------------------------------------(7)
         <char>| --------------------------------------------------(8)
         λ --------------------------------------------------------(9)      
|#

;; <string> can be single characters or mltiple characters
; we also define <chars> to generate more than one character

#|
<string> ::= (string <char> <chars>)| ----------------------------(10)
             (string <char>) -------------------------------------(11)
|#

#|
<chars> ::= <char> <chars>| ------------------------------------(12)
            <char> ---------------------------------------------(13)
|#

;; <string-insertSE> can be single <SE> or mltiple <SE>
; we also define <other> to generate more than one <SE>

#|
<string-insertSE> ::= (string-insert <SE> <other>)| --------------(14)
                      (string-insert <SE>)| ----------------------(15)
                      (string-insert) ----------------------------(16)
|#

#|
<other> ::= <SE> <other>| ----------------------------------------(17)
            <SE> -------------------------------------------------(18)
|#

;; <string-appendSE> can be single string (<string> or <D>) or mltiple strings
; we also define <strings> to generate more than one string
#|
<string-appendSE> ::= (string-append <strings>)| -----------------(19)
                      (string-append) ----------------------------(20)
|#

#|
<strings> ::= <string> <strings>| --------------------------------(21)
              <D> <strings>| -------------------------------------(22)
              <string-appendSE> <strings>| -----------------------(23)
              <numString> <strings>| -----------------------------(24)
              <string-insertSE> <strings>| -----------------------(25)
              <string>| ------------------------------------------(26)
              <D>| -----------------------------------------------(27)
              <string-appendSE>| ---------------------------------(28)
              <string-insertSE>| ---------------------------------(29)
              <numString> ----------------------------------------(30)
|#

; <numString> is always a single number <num> or <string-lengthSE>

#|
<numString> ::= (number->string <num>)| --------------------------(31)
                (number->string <string-lengthSE>)| --------------(32)

|#

; <string-lengthSE> is always a <D>

#|
<string-lengthSE> ::= (string-length <strings>) ------------------(33)
|#


#|
----------------------------------Question 1.b----------------------------------
|#

; we will derive (string-append "69" (number->string (string-length "666")))

#|
<SE> (4)=> <string-appendSE> (19)=> (string-append <strings>) (22)=> (string-append <D> <strings>) (30)=> (string-append <D> <numString>)
     (32)=> (string-append <D> (number->string <string-lengthSE>)) (33)=> (string-append <D> (number->string (string-length <strings>)))
     (27)=> (string-append <D> (number->string (string-length <D>))) => (string-append "69" (number->string (string-length "666")))
|#

; we will derive (string-append (string-insert (string-append (string #\1 #\2) "12") 69) (number->string (string-length "666")))

#|
<SE> (4)=> <string-appendSE> (19)=> (string-append <strings>) (25)=> (string-append (<string-insertSE> <strings>)) (14)=> (string-append ((string-insert <SE> <other>) <strings>))
     (4)=> (string-append ((string-insert <string-appendSE> <other>) <strings>)) (19)=> (string-append ((string-insert (string-append <strings>) <other>) <strings>))
     (21)=> (string-append ((string-insert (string-append (<string> <strings>)) <other>) <strings>))
     (10)=> (string-append ((string-insert (string-append ((string <char> <chars>) <strings>)) <other>) <strings>))
     (13)=> (string-append ((string-insert (string-append ((string <char> <char>) <strings>)) <other>) <strings>))
     (27)=> (string-append ((string-insert (string-append ((string <char> <char>) <D>)) <other>) <strings>))
     (18)=> (string-append ((string-insert (string-append ((string <char> <char> ) <D>)) <SE>) <strings>))
     (2)=> (string-append ((string-insert (string-append ((string <char> <char>) <D>)) <num>) <strings>))
     (30)=> (string-append ((string-insert (string-append ((string <char> <char>) <D>)) <num>) (<numString>)))
     (32)=> (string-append ((string-insert (string-append ((string <char> <char>) <D>)) <num>) ((number->string <string-lengthSE>))))
     (33)=> (string-append ((string-insert (string-append ((string <char> <char>) <D>)) <num>) ((number->string (string-length <strings>)))))
     (27)=> (string-append ((string-insert (string-append ((string <char> <char>) <D>)) <num>) ((number->string (string-length <D>)))))
     => (string-append ((string-insert (string-append ((string #\1 #\2) "12")) 69) ((number->string (string-length "666")))))
|#

;we will derive (string-append (string-insert) (string-append))

#|
<SE> (4)=> <string-appendSE> (19)=> (string-append <strings>) (25)=> (string-append <string-insertSE> <strings>) (16)=> (string-append (string-insert) <strings>)
     (28)=> (string-append (string-insert) <string-appendSE>) (20)=> (string-append (string-insert) (string-append))
|#


#|
----------------------------------Question 2----------------------------------
|#

;; first we define our f function
; which takes a square of a number and add the second number
(: f : Number Number -> Number)
(define (f x y)
  (+ (* x x) y))

;; now we defin sum-of-squares which takes a list of numbers
;; and return the sum of squares
; we start the sum from zero and useing f function to add the squares.
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl f 0 lst))

; some tests
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(1 1 1 1 1 1 1)) => 7)
(test (sum-of-squares '()) => 0)



#|
----------------------------------Question 3.a----------------------------------
|#

(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
 (: poly : (Listof Number) Number Integer Number -> Number)
 (define (poly argsL x power accum)
   (cond
     [(= (length argsL) 1) (+ accum (* (first argsL) (expt x power)))]
     [else (+ (poly (rest argsL) x (+ power 1) (* (first argsL) (expt x power))) accum)]))
 (: polyX : Number -> Number)
 (define (polyX x)
   (poly coeffs x 0 0))
 polyX)

(define p123 (createPolynomial '(1 2 3)))
(test (p123 -2) => 9)


#|
----------------------------------Question 3.b.1----------------------------------
|#

#|
<PLANG> ::= 
<AEs> ::= 
<AE> ::= num|
         
|#