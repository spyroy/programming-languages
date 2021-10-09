#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <-- fill in -->}
        |  { union <-- fill in --> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | <-- fill in --> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET]
    [Smult <-- fill in -->]
    [Inter <-- fill in -->]
    [Union <-- fill in -->]
    [IdS    Symbol]
    [WithS  Symbol SOL SOL])


;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) <-- fill in --> ] 
    [(symbol: name) <-- fill in -->]
    [(cons 'with more)
     (<-- fill in --> 
          [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) <-- fill in -->]
    [(list 'intersect lhs rhs) <-- fill in -->]
    [<-- fill in -->]
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))

  
(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")




;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  <-- fill in -->)

(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)

(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  <-- fill in -->)

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())

(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (<-- fill in --> (sort l <)))

(test (create-sorted-set '(3 4 5 3)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())

(: set-union : SET SET -> SET)
(define (set-union A B)
  ( <-- fill in -->))

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))

(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter <-- fill in -->))

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())

(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
  ( <-- fill in -->))

(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
;; (test (set-smult 0 '(3 4 5)) => '(0 0 0)) ;; This test is invalid. We assume the scalar is >=1

;;-----------------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`set' is a { <NumList> }, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      set[v/x]              = set 
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) <-- fill in -->]
    [(Smult n s) (Smult n (subst s from to))]
    [(Inter l r) (Inter <-- fill in -->)]
    [(Union l r) <-- fill in -->]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body) <-- fill in -->]))

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    eval({ N1 N2 ... Nl })  = sort( create-set({ N1 N2 ... Nl }))
                               ;; where create-set removes all duplications from
                                  the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) = { K*N1 K*N2 ... K*Nl }
                               ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2}) = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})     = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({with {x E1} E2})  = eval(E2[eval(E1)/x])
    eval(id)                = error!
|#



;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr)
  (cases expr
    [(Set S) <-- fill in -->]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult <-- fill in -->)]
    [(Inter l r) (set-intersection <-- fill in -->)]
    [(Union l r) <-- fill in -->]
    [(WithS name named body) <-- fill in -->]
    [(IdS name) <-- fill in -->]))

(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str))
    
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")