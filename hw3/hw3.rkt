#lang plai
;; symbol<? : symbol symbol -> boolean
;; check which string of symbol is faster on alphabetical order
(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(test (symbol<? 'a 'b) #t)
(test (symbol<? 'a 'a) #f)
(test (symbol<? 'aa 'a) #f)
(test (symbol<? 'aa 'ab) #t)
(test (symbol<? 'ac 'a) #f)
(test (symbol<? 'aa 'aa) #f)

;; define type WAE
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])
(define w_num (num 5))
(define w_name (id 'a))
(define w_w (with 'name w_num w_name))
(test (with-name w_w) 'name)
(test (with-named-expr w_w) (num 5))

;; parse : list of symbols -> WAE
;; change list of symbols into WAE
(define (parse sexp)
(match sexp
[(? number?) (num sexp)]
[(list '+ l r) (add (parse l) (parse r))]
[(list '- l r) (sub (parse l) (parse r))]
[(list 'with (list x i) b) (with x (parse i) (parse b))]
[(? symbol?) (id sexp)]
[else (error 'parse "bad syntax" sexp)]))

(test (parse '{+ 5 4}) {add (num 5) (num 4)})
(test (parse '{- 5 4}) {sub (num 5) (num 4)})
(test (parse '6) (num 6))
(test/exn (parse '{+ 5 4 12}) "bad syntax")

;; free-ids : WAE -> list-of-sym
;; find free identifiers and list them alphabetical order
(define (free-ids wae)
  (type-case  WAE wae
    [num (n) '()]
    [id (s) (list s)]
    [with (x expr body)
          (sort (remove-duplicates (remove* (list x) 
                                            (append 
                                              (free-ids body) 
                                              (free-ids expr)))) 
                symbol<?)]
    [add (l r) (sort (remove-duplicates (append (free-ids l) 
                                                (free-ids r))) 
                     symbol<?)]
    [sub (l r) (sort (remove-duplicates (append (free-ids l) 
                                                (free-ids r))) 
                     symbol<?)]))

(test (free-ids (parse '{with {x 5} {+ y z}})) '{y z})
(test/exn (free-ids (parse '{with x {+ y z} })) "bad syntax")
(test (free-ids (parse '{with {x {+ 1 2}} {with {y {- 4 2}} {+ x x}}}) ) '())
(test (free-ids (parse '{with {x {+ a 2}} {with {y {- 4 2}} {+ z c}}}) ) '(a c z))
(test (free-ids (parse '{+ {with {x {+ 1 2}} {+ x x}} {with {x {- 4 3}} {+ x x}}})) '())
(test (free-ids (parse '{+ {with {x {+ d 1}} {+ z x}} {with {x {- 4 3}} {+ a x}}})) '(a d z))
(test (free-ids (parse '{- {with {x {+ d 1}} {+ z x}} {with {x {- 4 3}} {+ a x}}})) '(a d z))
(test/exn (free-ids (parse '{+ {with {x {+ d 1}} {+ z x}} {with z {+ a x}}})) "bad syntax")
(test (free-ids (parse 5)) '())
(test (free-ids (parse 'a)) '())

;; binding-ids : WAE -> list-of-sym
;; find binding identifiers and list them alphabetical order
(define (binding-ids wae)
   (type-case WAE wae
    [num (n) '()]
    [id (s) '()]
    [with (x expr body)
          (sort (remove-duplicates (append (binding-ids body) (binding-ids expr) (list x))) symbol<?)]
    [add (l r) (sort (remove-duplicates (append (binding-ids l) (binding-ids r))) symbol<?)]
    [sub (l r) (sort (remove-duplicates (append (binding-ids l) (binding-ids r))) symbol<?)]))

(test (binding-ids (parse '{with {x 5} {+ y z}})) '{x})
(test/exn (binding-ids (parse '{with x {+ y z}})) "bad syntax")
(test (binding-ids (parse '{with {x {+ 1 2}} {with {y {- 4 2}} {+ x x}}})) '(x y))
(test (binding-ids (parse '{with {x {+ 1 2}} {with {y {- 4 2}} {+ 1 x}}}) ) '(x y))
(test (binding-ids (parse '{with {x {+ a 2}} {with {y {- 4 2}} {+ z c}}}) ) '(x y))
(test (binding-ids (parse '{with {x {+ a 2}} {- {- 4 2} {+ z c}}}) ) '(x))
(test (binding-ids (parse '{+ {with {x {+ 1 2}} {+ x x}} {with {x {- 4 3}} {+ x x}}})) '(x))
(test (binding-ids (parse '{+ {with {x {+ d 1}} {+ z x}} {with {x {- 4 3}} {+ a x}}})) '(x))
(test (binding-ids (parse '{- {with {x {+ d 1}} {+ z x}} {with {x {- 4 3}} {+ a x}}})) '(x))
(test/exn (binding-ids (parse '{+ {with {x {+ d 1}} {+ z x}} {with z {+ a x}}})) "bad syntax")
(test (binding-ids (parse 5)) '())
(test (binding-ids (parse 'a)) 'a)

;; bound-ids : WAE -> list-of-sym
;; find bound identifiers and list them alphabetical order
(define (bound-ids wae)
  (define (intersect l1 l2)
    (cond 
      [(or (empty? l1) (empty? l2)) '()]
      [(eq? (first l1) (first l2)) (cons (first l1) (intersect (rest l1) l2))]
      [(not (eq? (first l1) (first l2))) (intersect (rest l1) l2)]))
  (define (later wae)
    (type-case WAE wae
               [num (n) '()]
               [id (s) '()]
               [with (x expr body)
                     (sort (remove-duplicates  (append (later body)
                                                       (later expr)))
                           symbol<?)]
               [add (l r) (sort (remove-duplicates (append (later l)
                                                           (later r)))
                                symbol<?)]
               [sub (l r) (sort (remove-duplicates (append (later l)
                                                           (later r)))
                                symbol<?)]))
  (intersect (later wae) (binding-ids wae))
  )



(test (bound-ids (parse '{with {x 5} {+ y z}})) '{})
(test/exn (bound-ids (parse '{with x {+ y z}})) "bad syntax")
(test (bound-ids (parse '{with {x {+ 1 2}} {with {y {- 4 2}} {+ x x}}}) ) '(x))
(test (bound-ids (parse '{with {x {+ a 2}} {with {y {- 4 2}} {+ z c}}}) ) '())
(test (bound-ids (parse '{+ {with {x {+ 1 2}} {+ x x}} {with {x {- 4 3}} {+ x x}}})) '(x))
(test (bound-ids (parse '{+ {with {x {+ d 1}} {+ z x}} {with {x {- 4 3}} {+ a x}}})) '(x))
(test/exn (bound-ids (parse '{+ {with {x {+ d 1}} {+ z x}} {with z {+ a x}}})) "bad syntax")
(test (bound-ids (parse '{- {with {x {+ d 1}} {+ z x}} {with {x {- 4 3}} {+ a x}}})) '(x))
(test (bound-ids (parse '{- {with {x {with {x {with {y {with {y {- 4 3}} {+ y x}}} {+ a x}}} {+ a x}}} {+ z x}} {with {x {- 4 3}} {+ a x}}})) '(x y))
(test (bound-ids (parse 5)) '())
(test (bound-ids (parse 'a)) '())


(test (bound-ids (parse '{with {x 5} {+ z {- y z}}})) empty) 
(test (binding-ids (parse '{with {x 5} {+ z {- y x}}})) (list 'x))
(test (free-ids (parse '{with {x 5} {+ x {- y x}}})) (list 'y))
