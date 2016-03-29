#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; new-number? : sth -> boolean
;; check whether number or list of number
(define (new-number? n) (or (number? n) ((non-empty-listof number?) n)))
(test (new-number? 1) #t)
(test (new-number? -1) #t)
(test (new-number? (list 1 2 3)) #t)
(test (new-number? empty) #f)
(test (new-number? (list empty)) #f)
(test (new-number? 'a) #f)

;; bin-op : (number number -> number) (listof number or number) (listof number or number) -> (listof number))
;; applies a binary numeric function on all combinations of numbers from
;; the two input lists or numbers, and return the list of all of the results
(define (bin-op op ls rs)
  (define (helper l rs)
    ;; f : number -> number
    (if (list? rs)
        (map (lambda (x) (op l x)) rs)
        (list (op l rs))))
  (if (null? ls)
    null
    (if (list? ls)
        (append (helper (first ls) rs) (bin-op op (rest ls) rs))
        (helper ls rs))))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")

;; WAE abstract syntax trees
(define-type WAE
  [num  (num new-number?)]
  [add  (left WAE?) (right WAE?)]
  [sub  (left WAE?) (right WAE?)]
  [with (name symbol?) (init WAE?) (body WAE?)]
  [pooh (input list?) (op symbol?)]
  [id   (name symbol?)])

; parse-sexpr : sexpr -> WAE
;; to convert s-expressions into WAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? new-number?) (num sexp)]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(list 'pooh a ... op) (pooh (map parse-sexpr a) op)]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax: ~a" sexp)]))

;; parses a string containing a WAE expression to a WAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (type-case WAE expr
    [num (n)   expr]
    [add (l r) (add (subst l from to) (subst r from to))]
    [sub (l r) (sub (subst l from to) (subst r from to))]
    [pooh (l op) (pooh (map (lambda (x) (subst x from to)) l) op)]
    [id (name) (if (symbol=? name from) (num to) expr)]
    [with (bound-id named-expr bound-body)
          (with bound-id
                (subst named-expr from to)
                (if (symbol=? bound-id from)
                    bound-body
                    (subst bound-body from to)))]))


;; evaluates WAE expressions by reducing them to numbers
(define (eval expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (bin-op + (eval l) (eval r))]
    [sub (l r) (bin-op - (eval l) (eval r))]
    [pooh (l op) (cond
                     [(symbol=? op '+) 
                      (if (= (length l) 1)
                          (eval (first l))
                          (if (= (length l) 2)
                              (bin-op + (eval (first l)) (eval (second l)))
                              (bin-op + (bin-op + (eval (first l)) (eval (second l))) (eval (pooh (rest (rest l)) op)))))]
                     [(symbol=? op '-) 
                      (if (= (length l) 1)
                          (eval (first l))
                          (if (= (length l) 2)
                              (bin-op - (eval (first l)) (eval (second l)))
                              (bin-op - (bin-op - (eval (first l)) (eval (second l))) (eval (pooh (rest (rest l)) '+)))))])]
    [with (bound-id named-expr bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval named-expr)))]
    [id (name) (error 'eval "free identifier: ~s" name)]))

; run : string -> listof number
;; evaluate a WAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "5") 5)
(test (run "{+ 5 {1 2 3}}") '{6 7 8})
(test (run "{+ {2 1} {3 4}}") '(5 6 4 5))
(test (run "{+ {- {+ 1 3} 2} {10 -10}}") '(12 -8))
(test (run "{+ 5 5}") '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") '(20))
(test (run "{with {x 5} {+ x x}}") '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") '(10))
(test (run "{with {x 5} {with {y x} y}}") 5)
(test (run "{with {x 5} {with {x x} x}}") 5)
(test/exn (run "{with {x 1} y}") "free identifier")
(test (run "{+ 3 7}") '(10))
(test (run "{- 10 {3 5}}") '(7 5))
(test (run "{with {x {+ 5 5}} {+ x x}}") '(20))

;; additional tests for complete coverage
(test (run "{with {x 2} {- {+ x x} x}}") '(2))
(test/exn (run "{with x = 2 {+ x 3}}") "bad syntax")
(test/exn (run "{bleh}") "bad syntax")

(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 -}") '(-1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {x 20} {pooh 1 x +}}} {with {y 10} {pooh x y -}}}") '(11))
(test (run "{with {x {pooh 1 2 3 4 5 +}} x}") '(15))
(test (run "{pooh {with {x {pooh {1 2} {3 4} 1 +}} x} 2 3 -}") '(0 1 1 2))
(test (run "{pooh 1 2 3 4 5 +}") '(15))
(test (run "{pooh {1 2 3} {4 5} -}") '(-3 -4 -2 -3 -1 -2))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 3 4 +}") '(10))
(test (run "{pooh {3 4} {-4 0} 5 +}") '(4 8 5 9))
(test (run "{pooh 1 2 3 4 -}") '(-8))
(test (run "{pooh 1 2 3 4 5 6 -}") '(-19))
(test (run "{pooh 1 2 3 4 5 6 7 8 9 -}") '(-43))
(test (run "{pooh {4 1} 1 {5 6 7} -}") '(-2 -3 -4 -5 -6 -7))
(test (run "{+ {pooh 1 {4 9} -3 {2 0 1} +} {- {pooh {3 4} {2} -} 4}}") '(1 2 -1 0 0 1 6 7 4 5 5 6))
(test (run "{pooh 1 {pooh 1 2 -} 3 +}") '(3))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh {2 1} {3 4} +}") '(5 6 4 5))
(test (run "{with {x {1 2}} {pooh x {+ {1 2} 1} -}}") '(-1 -2 0 -1))
(test (run "{with {x {1 2}} {pooh x {pooh {1 2} 1 +} -}}") '(-1 -2 0 -1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {y {1 -2}} {pooh 1 y 2 -}}} {+ x x}}") '(-4 -1 -1 2))

(test (run "{with {x {with {x 3} {pooh 1 2 x x +}}} {pooh x x -}}") '(0))
(test (run "{with {x {with {x {2 3}} {pooh 1 2 x +}}} {pooh x 1 -}}") '(4 5))
(test (run "{with {x {with {y {2 3}} {pooh 1 2 y +}}} {pooh x 1 -}}") '(4 5))
(test (run "{with {x {1 2}} {with {y {2 3}} {+ {pooh 1 2 y +} {pooh x y +}}}}") '(8 9 9 10 9 10 10 11))
(test (run "{with {x {1 2}} {pooh {with {x 1} {+ x 1}} x -}}") '(1 0))
(test (run "{with {x {1 2}} {pooh {with {x {1 2}} {+ x 1}} x +}}") '(3 4 4 5))
(test (run "{with {x {1 2}} {+ {with {x 1} {pooh x x x x x +}} x}} ") '(6 7))
(test (run "{with {x {1 2}} {+ {with {x x} {pooh x 1 -}} x}} ") '(1 2 2 3))
(test (run "{with {x {1 2}} {- {with {x x} {pooh x 1 -}} x}} ") '(-1 -2 0 -1))

; exception test
(test/exn (run "{with {x 1}}") "parse: bad syntax: (with (x 1))")
(test/exn (run "{with {x {1 x}} x}")  "parse: bad syntax: (1 x)")
(test/exn (run "{with {x {}} x}") "parse: bad syntax: ()")
(test/exn (run "{with {x 1} y}") "eval: free identifier: y")

;other's test
(test (run "{pooh 1 +}")  '(1))
(test (run "{pooh 1 -}")  '(1))
(test (run "{+ {pooh 1 2 -} 5}") '(4))
(test (run "{pooh {pooh 1 2 3 +} {pooh 1 2 {2 3} -} {2 3} +}") '(5 6 4 5))
(test (run "{pooh {with {x {with {y {pooh 1 {2 3} -}} {+ y {3 4}}}} {+ x {2 3}}} 5 +}") '(9 10 10 11 8 9 9 10))
(test (run "{+ 1 {2 3}}") '(3 4))
(test (run "{pooh {with {x 1} {pooh {+ x 1} {+ x 2} {+ x 3} +}} 2 3 4 +}") '(18))
(test (run "{+ {pooh 1 2 3 +} {pooh {1} {2 3} {4 5 6} +}}") '(13 14 15 14 15 16))
(test/exn (run "{with {x x} x}") "eval: free identifier: x")
(test (run "{with {x {+ 1 {5 2}}} {- {with {y 1} {pooh x y y +}} x}}") '(2 5 -1 2))