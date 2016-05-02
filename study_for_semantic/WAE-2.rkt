#lang plai

(define-type WAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (name-expr WAE?) (body WAE?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list x expr) body) (with x (parse expr) (parse body))]))

(define (subst body x expr)
  (type-case WAE body
    [num (n) body]
    [id (s) (if (symbol=? s x)
                (num expr)
                body)]
    [add (l r) (add (subst l x expr) (subst r x expr))]
    [sub (l r) (sub (subst l x expr) (subst r x expr))]
    [with (x_ expr_ body_) (with x_ (subst expr_ x expr) (if (symbol=? x x_)
                                                             body_
                                                             (subst body_ x expr)))]))

(define (interp wae)
  (type-case WAE wae
    [num (n) n]
    [id (s) (error "free identifier")]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [with (x expr body) (interp (subst body x (interp expr)))]))

(test (interp (parse '{with {x {+ 1 4}} {+ x x}})) 10)
