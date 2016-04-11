#lang plai
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [id (name symbol?)]
  [with (name symbol?)
        (named-expr WAE?)
        (body WAE?)])

;; parse : sexp -> WAE
;; convert s-expression into WAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(= 3 (length sexp))
         (if (symbol=? (first sexp) '+)
             (add (parse (second sexp)) (parse (third sexp)))
             (if (symbol=? (first sexp) '-)
                 (sub (parse (second sexp)) (parse (third sexp)))
                 (if (symbol=? (first sexp) 'with)
                     (with (first (second sexp)) (parse (second (second sexp))) (parse (third sexp)))
                     (error "You should put right format"))))]
    [else (error "You should put right format")]))

(test (parse '3) (num 3))
(test/exn (parse '()) "You should put right format")
(test (parse 'a) (id 'a))
(test (parse '{+ 3 {+ 4 {- 5 {with {x 5} {+ x 5}}}}}) (add (num 3) (add (num 4) (sub (num 5) (with 'x (num 5) (add (id 'x) (num 5)))))))
;; substitute : WAE symbol WAE -> WAE
;; substitute x in expression 
(define (subst body x expr)
  (type-case WAE body
    [num (n) body]
    [add (l r) (add (subst l x expr) (subst r x expr))]
    [sub (l r) (sub (subst l x expr) (subst r x expr))]
    [id (s) (if (symbol=? s x)
                (num expr)
                (id s))]
    [with (x_ expr_ body_) (with x_ 
                                 (subst expr_ x expr)
                                 (if (symbol=? x x_)
                                     body_
                                     (subst body_ x expr)))])) 

;; interp : WAE -> number
;; interpret WAE
(define (interp wae)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [id (s) (error "free variable")]
    [with (x expr body) (interp (subst body x (interp expr)))]))

(test (interp (parse '{+ 3 {with {x 5} {+ x x}}})) 13)
(test (interp (parse '{with {x 1} {with {x 5} {+ x x}}})) 10)
(test (interp (parse '{with {x 1} {with {y 5} {+ x y}}})) 6)
(test (interp (parse '{with {x 1} {+ {with {x 2} x} 1}})) 3)