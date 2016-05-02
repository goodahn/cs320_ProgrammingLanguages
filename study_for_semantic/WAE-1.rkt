#lang plai
(define-type WAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [with (name symbol?)
        (named-expr WAE?)
        (body WAE?)])

;; parse : sexp -> WAE
(define (parse sexp)
  (cond
    [(number? sexp) 
     (num sexp)]
    [(symbol? sexp) 
     (id sexp)]
    [(and (= (length sexp) 3)
          (symbol=? '+ (first sexp)))
     (add (parse (second sexp)) 
          (parse (third sexp)))]
    [(and (= (length sexp) 3) 
          (symbol=? '- (first sexp)))
     (sub (parse (second sexp)) 
          (parse (third sexp)))]
    [(and (= (length sexp) 3) 
          (symbol=? 'with (first sexp)))
     (with (first (second sexp)) 
           (parse (second (second sexp))) 
           (parse (third sexp)))]))

;; subst : body x expr
;; convert x that is in body to expr
(define (subst body x expr)
  (type-case WAE body
    [num (n) body]
    [add (l r) (add (subst l x expr) 
                    (subst r x expr))]
    [sub (l r) (sub (subst l x expr) 
                    (subst r x expr))]
    [id (s) (if (symbol=? s x)
                (num expr)
                (id s))]
    [with (x2 expr2 body2) (with x2 (subst expr2 x expr)
                                 (if (symbol=? x x2)
                                     body2
                                     (subst body2 x expr)))]))

                            
;; interp : WAE -> number
(define (interp wae)
  (type-case WAE wae
    [num (n) n]
    [id (s) 
        (error "No free variable")]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [with (x expr body) (interp (subst body x (interp expr)))]))
(test (interp (parse '{with {x 5} {with {x 5} {+ x x}}})) 10)
(test (interp (parse '{with {x 5} {with {y 5} {+ x y}}})) 10)
(test (interp (parse '{with {x 5} {with {y 5} {with {z 5} {with {k 1} {+ x y}}}}})) 10)
(test (interp (parse '{with {x 5} {with {y 5} {with {z 5} {with {k {with {y 1} {+ x y}}} {+ k 10}}}}})) 16)
(test/exn (interp (parse '{with {x 5} {with {x 5} {+ x y}}})) "No free variable")
