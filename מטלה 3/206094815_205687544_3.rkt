#lang pl

#|----------------------------------Question A.1----------------------------------

;; we define the grammer SOL the Set Operation Language

<SOL> :: = { <NumList> } ---------------------------(1)
        |  { scalar-mult <num> <SOL> } -------------(2)
        |  { intersect <SOL> <SOL>} ----------------(3)
        |  { union <SOL> <SOL> } -------------------(4)
        |  <id> ------------------------------------(5)
        |  { with {<id> <SOL> } <SOL> } ------------(6) ;; this should be a syntactic sugar
       
<NumList> :: =  λ ----------------------------------(8)    ;; where λ stands for the empty word, i.e., { } is the empty set
            | <num> <nums> ---------------------(9)
            | <num> ---------------------------(10)

<nums> :: = <num> <nums> --------------------------(11)
         |  <num> ---------------------------------(12)

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol

;; lets try to derive "{with {S {intersect {1 2 1 3 7 3} {union {1 2 3} {4 2 3}}}}{union S S}}"
#|

<SOL> (6)=> { with {<id> <SOL> } <SOL> } (3)=> { with {<id> { intersect <SOL> <SOL>} } <SOL> }
      (1)=> { with {<id> { intersect { <NumList> } <SOL>} } <SOL> } (9)=> { with {<id> { intersect { <num> <nums> } <SOL>} } <SOL> }
      (11)=> { with {<id> { intersect { <num> <num> <nums> } <SOL>} } <SOL> } (11)=> { with {<id> { intersect { <num> <num> <num> <nums> } <SOL>} } <SOL> }
      (11)=> { with {<id> { intersect { <num> <num> <num> <num> <nums> } <SOL>} } <SOL> } (11)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <nums> } <SOL>} } <SOL> }
      (12)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } <SOL>} } <SOL> }
      (4)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union <SOL> <SOL> }} } <SOL> }
      (1)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <NumList> } <SOL> }} } <SOL> }
      (9)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <nums> } <SOL> }} } <SOL> }
      (11)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <nums> } <SOL> }} } <SOL> }
      (12)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } <SOL> }} } <SOL> }
      (1)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } { <NumList> } }} } <SOL> }
      (9)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } { <num> <nums> } }} } <SOL> }
      (11)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } { <num> <num> <nums> } }} } <SOL> }
      (12)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } { <num> <num> <num> } }} } <SOL> }
      (4)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } { <num> <num> <num> } }} } { union <SOL> <SOL> } }
      (5)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } { <num> <num> <num> } }} } { union <id> <SOL> } }
      (5)=> { with {<id> { intersect { <num> <num> <num> <num> <num> <num> } { union { <num> <num> <num> } { <num> <num> <num> } }} } { union <id> <id> } }
       => { with {S { intersect { 1 2 1 3 7 3 } { union { 1 2 3 } { 4 2 3 } }} } { union S S } }

|#

 
|#

;; The abstract syntax tree SOL
(define-type SET = (Listof Number))

(define-type SOL
    [Set  SET]
    [Smult Number SOL]
    [Inter SOL SOL]
    [Union SOL SOL]
    [IdS    Symbol]
    [WithS  Symbol SOL SOL])

;; ----------------------------------Question A.2----------------------------------
;; Parser

;; to convert s-expressions into SOLs
;; if we got list of numbers so we call constructor Set
;; if its a symbol we call constructor Id (although this is not in our language, we will throw error on the evaluation part)
;; if it's 'with' case we check for syntax and call With constructor
;; if its a scalar-mult case we call constructor Smult
;; if its a intersect case we call constructor Inter
;; if its a union case we call constructor Union
; else we throw error
(: parse-sexprS : Sexpr -> SOL)
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns) ] 
    [(symbol: name) (IdS name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named-expr) body)
        (WithS name (parse-sexprS named-expr) (parse-sexprS body))]
       [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs)(Smult sc (parse-sexprS rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs))]
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))]
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


;; parses a string containing a SOL expression to a SOL AST
(: parseS : String -> SOL)
(define (parseS str)
  (parse-sexprS (string->sexpr str)))


; some tests
(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "S") => (IdS 'S))
(test (parseS "{with {S {intersect {1 2 3} {4 2 3}}}{union S S}}") => (WithS 'S (Inter (Set '(1 2 3)) (Set '(4 2 3))) (Union (IdS 'S) (IdS 'S))))
(test (parseS "{ scalar-mult 3/2 {with {S {intersect {1 2 1 3 7 3} {union {1 2 3} {4 2 3}}}} {union S S}}}") =>
      (Smult 3/2 (WithS 'S (Inter (Set '(1 2 1 3 7 3)) (Union (Set '(1 2 3)) (Set '(4 2 3)))) (Union (IdS 'S) (IdS 'S)))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}} {union S S}}") =error> "parse-sexprS: bad `with' syntax in")
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{x y z}") =error> "bad syntax in")

#|
for this question we used a little bit of the code from Towards FLANG 1.rkt
|#






;; ----------------------------------Question A.3----------------------------------
;; Operations on SETs

;; ismember recives a number and a SET (listof Number)
;; and returns true (#t) if the number is in the SET
; otherwise false (#f).
(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond
    [(null? l) #f]
    [(equal? n (first l)) #t]
    [(not (null? (rest l))) (ismember? n (rest l))]
    [else #f]))

; some tests for ismember?
(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)
(test (ismember? 1 '(1 1 1)) => #t)
(test (ismember? 3 '(1 1 3)) => #t)

;; remove-duplicates recives a SET
; and returns a SET with no duplicates in it.
(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond
       [(null? l) '()]
       [(not (ismember? (first l) (rest l))) (cons (first l) (remove-duplicates (rest l)))]
       [else (remove-duplicates (rest l))]))

;some tests for remove-duplicates
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(1 1 1 1 1)) => '(1))
(test (remove-duplicates '(1 1 1 2 2 2 3 3 3)) => '(1 2 3))

;; sort recives SET and a boolean function
; and returns a sorted set.
(: sort : SET (Number Number -> Boolean) -> SET)
(define (sort l pred)
  (cond
    [(null? l) '()]
    [(null? (rest l)) l]
    [(pred (first l) (second l)) (cons (first l) (sort (rest l) pred))]
    [else (sort (cons (second l) (sort (cons (first l) (rest (rest l))) pred)) pred)]))

;; create-sorted-set recives SET
;; and returns a sorted SET with no duplicates.
;; first we remove the duplicates
; so our sort function won't need to deal with equal numbers
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (sort (remove-duplicates l) <))

;some tests for create-sorted-set
(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '( 6 6 5 5 4 4 3 3 2 2 1 1)) => '(1 2 3 4 5 6))

;; set-union recives two sets
;; and returns the union between them.
;; first we checked if one of the sets is null
;; if so the returned value will be the other set.
;; after that we checked if a number in A is in B
;; if so we recursively call the function with (rest A) B)
;; otherwise we need to call to the element that not present in B
;; with the union of (rest A) B)
; we also want it sorted and no duplicates so we call it with create-sorted-set
(: set-union : SET SET -> SET)
(define (set-union A B)
  (cond
    [(null? A) (create-sorted-set B)]
    [(null? B) (create-sorted-set A)]
    [(ismember? (first A) B) (create-sorted-set (set-union (rest A) B))]
    [else (create-sorted-set (cons (first A) (set-union (rest A) B)))]))

;some tests for set-union
(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6))
(test (set-union '(4 5 6) '(1 2 3)) => '(1 2 3 4 5 6))


;; set-intersection recives two sets
;; and returns the intersection between them.
;; we used [https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29]
; to know about filter.
(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (create-sorted-set (filter mem-filter B)))

;some tests for set-intersection
(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '(3 4 4 5) '(6 4 66 3 4)) => '(3 4))

;; set-smult recives a number
;; and returns a set with each number multiplied by the number.
; (not sure if it sould be sorted and with no duplicate???)
(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
  (cond
    [(null? l) '()]
    [else (create-sorted-set (cons (* (first l) n) (set-smult  n (rest l))))]))
  
;some tests for set-smult
(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 2 '(3 6 6 9)) => '(6 12 18))
(test (set-smult 2 '(9 6 6 3)) => '(6 12 18))
(test (set-smult 2 '(6 6 6 3)) => '(6 12))
;(test (set-smult 0 '(3 4 5)) => '(0 0 0)) invalid test




;; ----------------------------------Question A.4----------------------------------

;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`Set' is a <NumList>, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      Set[v/x]              = Set
      {Smult n E}[v/x]      = {Smult n E[v/x]}
      {Inter E1 E2}[v/x]    = {Inter E1[v/x] E2[v/x]}
      {Union E1 E2}[v/x]    = {Union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id 
           (substS named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substS bound-body from to)))]))

;some tests for subst
(test (substS (Set '( 1 2 3)) 'x (Set '(1 2 3))) => (Set '(1 2 3)))
(test (substS (Smult 2 (IdS 'x)) 'x (Set '(1 2 3))) => (Smult 2 (Set '(1 2 3))))
(test (substS (Smult 2 (IdS 'y)) 'x (Set '(1 2 3))) => (Smult 2 (IdS 'y)))
(test (substS (Inter (IdS 'x) (Set '(1 2 3))) 'x (Smult 2 (Set '(1 2)))) => (Inter (Smult 2 (Set '(1 2))) (Set '(1 2 3))))
(test (substS (Inter (IdS 'y) (Set '(1 2 3))) 'x (Smult 2 (Set '(1 2)))) => (Inter (IdS 'y) (Set '(1 2 3))))
(test (substS (Union (IdS 'x) (Set '(4 5 6))) 'x (Smult 2 (Set '(2 3)))) => (Union (Smult 2 (Set '(2 3))) (Set '(4 5 6))))
(test (substS (Union (IdS 'y) (Set '(4 5 6))) 'x (Smult 2 (Set '(2 3)))) => (Union (IdS 'y) (Set '(4 5 6))))
(test (substS (WithS 'x (Set '(4 5 6)) (Inter (IdS 'x) (IdS 'x))) 'x (Set '(1 2 3))) => (WithS 'x (Set '(4 5 6)) (Inter (IdS 'x) (IdS 'x))))
(test (substS (WithS 'y (Set '(4 5 6)) (Inter (IdS 'x) (IdS 'x))) 'x (Set '(1 2 3))) => (WithS 'y (Set '(4 5 6)) (Inter (Set '(1 2 3)) (Set '(1 2 3)))))

#|
pretty much like in Towards FLANG 1.rkt
|#

;; ----------------------------------Question A.5----------------------------------

;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    
    eval({ N1 N2 ... Nl })  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) =  (K*N1 K*N2 ... K*Nl) 
    eval({intersect E1 E2}) = (sort (create-set (set-intersection (eval(E1) , eval(E2))))
                
    eval({union E1 E2}) = (sort (create-set (eval(E1) , eval(E2))))
    eval({with {x E1} E2}) = eval(E2[eval(E1)/x]) 
|#



;;---------  the eval procedure ------------------------------

;; evaluates SOL expressions by reducing them to set values
(: eval : SOL -> SET)
(define (eval expr )
  (cases expr
    [(Set S) (create-sorted-set S)]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult n (eval set))]
    [(Inter l r) (set-intersection (eval l) (eval r))]
    [(Union l r) (set-union (eval l) (eval r))]
    [(WithS name named-expr body)
     (eval (substS body
                  name
                  (Set (eval named-expr))))]
    [(IdS name) (error 'eval "free identifier: ~s" name)]))

#|
Here we also used Towards FLANG 1.rkt
|#


;; ----------------------------------Question A.6----------------------------------

;; evaluate a SOL program contained in a string
(: run : String -> SET)
(define (run str)
  (eval (parseS str)))

;some tests for run
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}} {with {x {4 5 7 6 9 8 8 8}} {union x S}}}") => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}} {union {scalar-mult 3 B} {4 5 7 9 8 8 8}}}") =error> "eval: free identifier:")




;; ----------------------------------Question B----------------------------------

#|
<WAE> ::= <num>
 | { + <WAE> <WAE> }
 | { - <WAE> <WAE> }
 | { * <WAE> <WAE> }
 | { / <WAE> <WAE> }
 | { with { <id> <WAE> } <WAE> }
 | <id>
|#


;; WAE abstract syntax trees
(define-type WAE
  [Num Number]
  [Add WAE WAE]
  [Sub WAE WAE]
  [Mul WAE WAE]
  [Div WAE WAE]
  [Id Symbol]
  [With Symbol WAE WAE])

;; to convert s-expressions into WAEs
(: parse-sexpr : Sexpr -> WAE)
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;; parses a string containing a WAE expression to a WAE AST
(: parse : String -> WAE)
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(: subst : WAE Symbol WAE -> WAE)
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (if (eq? bound-id from)
         expr
         (With bound-id
               named-expr
               (subst bound-body from to)))]))


;; ismember recives a symbol and a SET (listof Symbol)
;; and returns true (#t) if the number is in the SET
; otherwise false (#f).
(: Ismember? : Symbol (Listof Symbol)  -> Boolean)
(define (Ismember? n l)
  (cond
    [(null? l) #f]
    [(equal? n (first l)) #t]
    [(not (null? (rest l))) (Ismember? n (rest l))]
    [else #f]))

;; remove-duplicates recives a SET
; and returns a SET with no duplicates in it.
(: Remove-duplicates : (Listof Symbol)  -> (Listof Symbol))
(define (Remove-duplicates l)
  (cond
       [(null? l) '()]
       [(not (Ismember? (first l) (rest l))) (cons (first l) (Remove-duplicates (rest l)))]
       [else (Remove-duplicates (rest l))]))


;; function that consumes an abstract syntax tree (WAE)
;; and returns null if there are no free instance, 
;; and a list of all the free instances otherwise – each appearing exactly once in the list 
(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  (Remove-duplicates (freeInstanceList-helper expr)))


;; helper function that recives WAE
;; and returns a list of symbols of free instances,
;; if there are none than the function will return null.
(: freeInstanceList-helper : WAE -> (Listof Symbol))
(define (freeInstanceList-helper expr)
 (cases expr
   [(Id name) (list name)]
    [(Num n) '()]
    [(Add l r) (append (freeInstanceList-helper l) (freeInstanceList-helper r))]
    [(Sub l r) (append (freeInstanceList-helper l) (freeInstanceList-helper r))]
    [(Mul l r) (append (freeInstanceList-helper l) (freeInstanceList-helper r))]
    [(Div l r) (append (freeInstanceList-helper l) (freeInstanceList-helper r))]
    [(With bound-id named-expr bound-body)
     (append (freeInstanceList-helper named-expr) (freeInstanceList-helper (subst bound-body bound-id (Num 0))))]
    ))


;some tests for freeInstanceList
(test (freeInstanceList (parse "w")) => '(w))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (With 'x (Num 2) (Div (Mul (Id 'x) (Num 3)) (Id 'y)))) => '(y))
(test (freeInstanceList (parse "{with {x 1} {with {y 2} {/ {* xx y} z}}}")) => '(xx z))
(test (freeInstanceList (parse "{+ z {+ x z}}")) => '(x z))
(test (freeInstanceList (With 'x (Num 2) (With 'x (Add (Id 'y) (Num 1)) (Id 'xx)))) => '(y xx))
;error tests
(test (freeInstanceList (parse "{with x {* 5 x}}")) =error> "bad `with' syntax in")
(test (freeInstanceList (parse "{+ x {with {xxx 2} 1} {- y 2}}")) =error> "bad syntax in")


#|
------------some remarks on the work------------
Matan Greenberg wrote part A and Roi Mash wrote part B
of course we helped each other in each part.
part A took about 5 hours and part B took about 6 hours,
total work time about 11 hours.
We tried to cover all cases in the tests and we mainly
used 'FLANG 1.rkt' for this work.
|#