#lang pl

#|
The FLANG grammer

  <FLANG> ::=  1 <num>
           |2 { + <FLANG> <FLANG> }
           |3 { - <FLANG> <FLANG> }
           |4 { * <FLANG> <FLANG> }
           |5 { / <FLANG> <FLANG> }
           |6 { with { <id> <FLANG>} <FLANG> }
           |7 <id>
           |8 { fun { <id> } <FLANG> }
           |9 { call <FLANG> <FLANG> }

where <num> stands for any Racket Number
and <id> stands for any Racket Symbol
|#



;; Defining the AST -- FLANG
(define-type FLANG
  [Num Number]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Mul FLANG FLANG]
  [Div FLANG FLANG]
  [With Symbol FLANG FLANG]
  [Id Symbol]
  [Fun Symbol FLANG] ;; parameter, body
  [Call FLANG FLANG]) ;; Func-expression, Argument

(: parse-sexpr : Sexpr -> FLANG)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sxp
       [(list 'with (list (symbol: name) named-expr) body)
        (With name (parse-sexpr named-expr) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad with syntax in ~s" sxp)])]
    [(cons 'fun more)
     (match sxp
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad fun syntax in ~s" sxp)])]
    [(list 'call fun-exp arg-exp) (Call (parse-sexpr fun-exp)
                                        (parse-sexpr arg-exp))]
    [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
    [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
    [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))

(: parse : String -> FLANG)
(define (parse code)
  (parse-sexpr (string->sexpr code)))



(test (parse "{fun {x} x}")
      => (Fun 'x (Id 'x)))

(test (parse "{fun {x} {/ x 5}}")
      => (Fun 'x (Div (Id 'x)
                      (Num 5))))
(test (parse "{call {fun {x} {/ x 5}} 8}")
      => (Call (Fun 'x (Div (Id 'x)
                      (Num 5)))
               (Num 8)))

(test (parse "{with {sqr {fun {x} {* x x}}}
                 {+ {call sqr 5}
                    {call sqr 6}}}")
      => (With 'sqr
               (Fun 'x (Mul (Id 'x) (Id 'x)))
               (Add (Call (Id 'sqr) (Num 5))
                    (Call (Id 'sqr) (Num 6)))))

(test (parse "{call {fun x {+ x x}} 5}")
        =error> "bad fun syntax")

(test (parse "{with {x {+ 4 2}}
                 {* x x}}") => (With 'x
                                     (Add (Num 4) (Num 2))
                                     (Mul (Id 'x) (Id 'x))))

(test (parse "{+ 4 2}") => (Add (Num 4) (Num 2)))
(test (parse "{with x {+ 4 2}
                 {* x x}}") =error> "bad with syntax")

(test (parse "4") => (Num 4))
(test (parse "{+ 3 4}") => (Add (Num 3) (Num 4)))
(test (parse "{+ 3 {- 5 4}}") => (Add (Num 3)
                                      (Sub (Num 5)
                                           (Num 4))))
(test (parse "{+ 2 3 4 5}") =error> "bad syntax")
(test (parse "{* 3 {/ 25 5}}") => (Mul (Num 3)
                                       (Div (Num 25)
                                            (Num 5))))



#|
[substitution, take 4a]  e[v/i]
  To substitute an identifier ‘i' in an expression ‘e' with an expression ‘v',
  replace all identifiers in ‘e' that have the same name ‘i' that are free
  by the expression ‘v'.

Formal specifications for subst:
    N[v/x]                = N

    {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}

    {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}

    {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}

    {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}

    y[v/x]                = y
    x[v/x]                = v

    {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
    {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}

    {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}

    {fun {y} E}[v/x]      = { fun {y} E[v/x]}
    {fun {x} E}[v/x]      = { fun {x} E}
|#

 
(: subst : FLANG Symbol FLANG -> FLANG)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to) ;; returns expr[to/from]
  (cases expr
    [(Num n) expr] 
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(With name named-expr body)
     (With name 
           (subst named-expr from to)
           (if (eq? name from)
               body
               (subst body from to)))]
    [(Id name) (if (eq? name from) to expr)]
    [(Fun name body)
     (Fun name (if (eq? name from) body (subst body from to)))]
    [(Call fun-exp arg-exp) (Call (subst fun-exp from to)
                                  (subst arg-exp from to))]))


(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'x
             (Num 4))
      => (Fun 'x (Add (Id 'x) (Id 'y))))
(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'y
             (Num 4))
      => (Fun 'x (Add (Id 'x) (Num 4))))

(test (subst (Call (Fun 'x (Div (Id 'x)
                                (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
             'x
             (Num 3))
      => (Call (Fun 'x (Div (Id 'x)
                            (Id 'y)))
               (Add (Num 3) (Id 'y))))


(test (subst (Call (Fun 'x (Div (Id 'x)
                                (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
             'y
             (Num 3))
      => (Call (Fun 'x (Div (Id 'x)
                            (Num 3)))
               (Add (Id 'x) (Num 3))))

(test (subst (Add (Call (Id 'sqr) (Num 5))
                    (Call (Id 'sqr) (Num 6)))
             'sqr
             (Fun 'x (Mul (Id 'x) (Id 'x))))
      => (Add (Call (Fun 'x (Mul (Id 'x) (Id 'x))) (Num 5))
              (Call (Fun 'x (Mul (Id 'x) (Id 'x))) (Num 6))))




#| Formal specifications of eval:
  eval(N)         = N

  eval({+ E1 E2}) = eval(E1) + eval(E2)

  eval({- E1 E2}) = eval(E1) - eval(E2)

  eval({* E1 E2}) = eval(E1) * eval(E2)

  eval({/ E1 E2}) = eval(E1) / eval(E2)

  eval(id)        = error!

  eval({with {x E1} E2}) = eval(E2[eval(E1)/x])

  eval({fun {x} E}) = {fun {x} E}    == a cloud [param-name body]

  eval({call E1 E2}) = if {fun {x} Ef} <-- eval(E1)
                            eval (Ef[eval(E2)/x])
                       otherwise  Error!!!

|#


(: eval : FLANG -> FLANG)
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))] 
    [(With name named-expr body)
     (eval (subst body
                  name
                  (Num (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(test (eval (With 'x         ;; name
                  (Add (Num 4) (Num 2)) ;; named-expr
                  (Mul (Id 'x) (Id 'x)))) ;; body
      => 36)

(: run : String -> Number)
(define (run code)
  (eval (parse code)))


;; tests
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}}
              {+ x x}}") => 20)
(test (run "{with {x 5}
              {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}}
               {with {y {- x 3}}
                  {+ y y}}}") => 14)
(test (run "{with {x 5}
              {with {y {- x 3}}
                 {+ y y}}}") => 4)
(test (run "{with {x 5}
               {+ x {with {x 3}
                       10}}}") => 15)
(test (run "{with {x 5}
              {+ x {with {x 3}
                      x}}}") => 8)
(test (run "{with {x 5}
               {+ x {with {y 3}
                       x}}}") => 10)
(test (run "{with {x 5}
              {with {y x}
                 y}}") => 5)
(test (run "{with {x 5}
              {with {x {+ x 1}}
                 x}}") => 6)
(test (run "{with {x 1} y}") =error> "free identifier")


#|
{with {x {+ 4 2}}
  {* x {with {x 3}
         {* x x}}}


{* x {with {x {+ x 9}}
         {* x x}}
x
6



eval( {with {x E1} E2} ) =
1. v <- eval(E1)
2. E2' <- subst(E2, x, v) === E2[v/x]
3. eval (E2')

(eval (subst E2 x (eval E1)))




{call {fun {x}   ;; x is called the 'formal parameter/agument'
        {* x x}}
      5}         ;; 5 is called the 'actual parameter/agument'

{with {sqr {fun {x}
        {* x x}}}
   {+ {call sqr 5}
      {call sqr 6}}}

#|
The history of dealing with function in programming languages
1. First order: functions are not real values. They cannot be used or returned as
                values by other functions. This means that they cannot be stored
                in data structures. This is what most "conventional" languages used
                to have in the past.  
                An example of such a language is the Beginner Student language that
                is used in HtDP, where the language is intentionally first-order to
                help students write correct code
                (at the early stages where using a function as a value is usually
                an error).  It's hard to find practical modern languages that fall
                in this category.
2. Higher order: functions can receive and return other functions as values.
    This is what you get with C.
3.First class: functions are values with all the rights of other values. In
  particular, they can be supplied to other functions, returned from functions,
  stored in data structures, and new functions can be created at run-time.
  (And most modern languages have first class functions.)



x = b * b
y = 4 * a
y = y * c
x = x – y
x = sqrt(x)
y = -b
x = y + x
y = 2 * a
s = x / y


(-b + sqrt(b^2 - 4*a*c)) / 2a 


function foo(x) {
    function bar(y) { return x + y; }
    return bar;
  }
  function main() {
    var f = foo(1);
    var g = foo(10);
    alert(">> "+ f(2) + ", " + g(2));
  }

























(test (subst (Num 4)
             'x
             (Num 6))
  => (Num 4))
|#






|#








(test (subst (With 'y
                   (Add (Id 'x) (Num 1))
                   (Add (Id 'x) (Id 'y)))
             'x
             (Num 6))
      => (With 'y
               (Add (Num 6) (Num 1))
               (Add (Num 6) (Id 'y))))

(test (subst (With 'x
                   (Add (Id 'x) (Num 1))
                   (Add (Id 'x) (Id 'x)))
             'x
             (Num 6))
      => (With 'x
               (Add (Num 6) (Num 1))
               (Add (Id 'x) (Id 'x))))




(test (subst (Mul (Id 'x) (Id 'x)) 'x (Num 6))
      => (Mul (Num 6) (Num 6)))

(test (subst (Num 5) 'x (Num 6))
      => (Num 5))

(test (subst (Id 'x) 'x (Num 6))
      => (Num 6))

(test (subst (Id 'y) 'x (Num 6))
      => (Id 'y))

(test (subst (With 'x
                   (Num 3)
                   (Id 'x))
             'x
             (Num 6))
      => (With 'x
               (Num 3)
               (Id 'x)))

(test (subst (With 'y
                   (Num 3)
                   (Id 'x))
             'x
             (Num 6))
      => (With 'y
               (Num 3)
               (Num 6)))

;; Evaluation
#|
Today:
Defining substitutions:

  [substitution, take 1]  e[v/i]
  To substitute an identifier ‘i' in an expression ‘e' with an expression ‘v',
  replace all identifiers in ‘e' that have the same name ‘i' by the expression ‘v'.

{with {x 6} {* x x}}
e == {* x x}  ,  i == 'x  ,  v = 6
==> {* 6 6} 

{with {x 6} {* 5 8}}
e == {* 5 8}  ,  i == 'x  ,  v = 6
==> {* 5 8}

{with {x 6} {+ x {with {x 3} 10}}}
e == {+ x {with {x 3} 10}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {6 3} 10}}  Invalid expression!!!


Let us define a few terms for this:
1. Binding Instance: a binding instance of an identifier is one
   that is used to name it in a new binding.  In our <FLANG> syntax,
   binding instances are only the <id> position of the ‘with' form.

2. Scope: the scope of a binding instance is the region of program
   text in which instances of the identifier refer to the value bound
   in the binding instance.
   (Note that this definition actually relies on a definition
    of substitution, because that is what is used to specify how
    identifiers refer to values.)

3. Bound Instance (or Bound Occurrence): an instance of an identifier is
   bound if it is contained within the scope of a binding instance of its name.

4. Free Instance (or Free Occurrence): An identifier that is not contained in
   the scope of any binding instance of its name is said to be free.


[substitution, take 2]  e[v/i]
  To substitute an identifier ‘i' in an expression ‘e' with an expression ‘v',
  replace all identifiers in ‘e' that have the same name ‘i' that are not binding
 instances by the expression ‘v'.

{with {x 6} {* x x}}
e == {* x x}  ,  i == 'x  ,  v = 6
==> {* 6 6} 

{with {x 6} {* 5 8}}
e == {* 5 8}  ,  i == 'x  ,  v = 6
==> {* 5 8}

{with {x 6} {+ x {with {x 3} 10}}}
e == {+ x {with {x 3} 10}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {x 3} 10}} =eval> {+ 6 10} 


{with {x 6} {+ x {with {x 3} x}}}
e == {+ x {with {x 3} x}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {x 3} 6}} =eval> {+ 6 6} Not what we expected!!!

[substitution, take 3]  e[v/i]
  To substitute an identifier ‘i' in an expression ‘e' with an expression ‘v',
  replace all identifiers in ‘e' that have the same name ‘i', that are not binding
 instances, and that are not in any nested scope, by the expression ‘v'.

{with {x 6} {* x x}}
e == {* x x}  ,  i == 'x  ,  v = 6
==> {* 6 6} 

{with {x 6} {* 5 8}}
e == {* 5 8}  ,  i == 'x  ,  v = 6
==> {* 5 8}

{with {x 6} {+ x {with {x 3} 10}}}
e == {+ x {with {x 3} 10}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {x 3} 10}} =eval> {+ 6 10} 


{with {x 6} {+ x {with {x 3} x}}}
e == {+ x {with {x 3} x}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {x 3} x}} =eval> {+ 6 3} GOOD!

{with {x 6} {+ x {with {y 3} x}}}
e == {+ x {with {y 3} x}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {y 3} x}} =eval>  Undefined identifier!!!

[substitution, take 4]  e[v/i]
  To substitute an identifier ‘i' in an expression ‘e' with an expression ‘v',
  replace all identifiers in ‘e' that have the same name ‘i', that are not binding
 instances, and that are not in any nested scope of (a binding innstance of the name)
 `i', by the expression ‘v'.

{with {x 6} {* x x}}
e == {* x x}  ,  i == 'x  ,  v = 6
==> {* 6 6} 

{with {x 6} {* 5 8}}
e == {* 5 8}  ,  i == 'x  ,  v = 6
==> {* 5 8}

{with {x 6} {+ x {with {x 3} 10}}}
e == {+ x {with {x 3} 10}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {x 3} 10}} =eval> {+ 6 10} 


{with {x 6} {+ x {with {x 3} x}}}
e == {+ x {with {x 3} x}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {x 3} x}} =eval> {+ 6 3} GOOD!

{with {x 6} {+ x {with {y 3} x}}}
e == {+ x {with {y 3} x}}  ,  i == 'x  ,  v = 6
==> {+ 6 {with {y 3} 6}} =eval>  {+ 6 6} GOOD!


[substitution, take 4a]  e[v/i]
  To substitute an identifier ‘i' in an expression ‘e' with an expression ‘v',
  replace all identifiers in ‘e' that have the same name ‘i' that are free
  by the expression ‘v'.


(: subst : )


{with {x {+ 4 2}}
   {* x x}}
eval( {with {x E1} E2} ) =
1. v <- eval(E1)
2. E2' <- subst(E2, x, v) === E2[v/x]
3. eval (E2')

(eval (subst E2 x (eval E1)))


|#



