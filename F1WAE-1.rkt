#lang plai

(define-type F1WAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (name-expr F1WAE?) (body F1WAE?)]
  [app (func symbol?) (arg F1WAE?)])

(define-type FunDef
  [deffun (fname symbol?) (param symbol?) (body F1WAE?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list x expr) body) (with x (parse expr) (parse body))]
    [(list f a) (app f (parse a))]))

(define (parse-func func)
  (match func
    [(list 'deffun (list f p) body) (deffun f p (parse body))]))
  
(define (subst body x expr)
  (type-case F1WAE body
    [num (n) body]
    [id (s) (if (symbol=? s x)
                (num expr)
                body)]
    [add (l r) (add (subst l x expr) (subst r x expr))]
    [sub (l r) (sub (subst l x expr) (subst r x expr))]
    [with (x_ expr_ body_) (with x_ (subst expr_ x expr) (if (symbol=? x x_)
                                                             body_
                                                             (subst body_ x expr)))]
    [app (f a) (app f (subst a x expr))]))

(define (lookup-fun f funcs)
  (if (empty? funcs)
      (error "there is no such a function")
      (if (symbol=? f (deffun-fname (first funcs)))
          (first funcs)
          (lookup-fun f (rest funcs)))))

(define (interp f1wae funcs)
  (type-case F1WAE f1wae
    [num (n) n]
    [id (s) (error "No free identifier")]
    [add (l r) (+ (interp l funcs) (interp r funcs))]
    [sub (l r) (- (interp l funcs) (interp r funcs))]
    [with (x expr body) (interp (subst body x (interp expr funcs)) funcs)]
    [app (f a) (local [(define fun (lookup-fun f funcs))]
                 (interp (subst (deffun-body fun) (deffun-param fun) (interp a funcs)) funcs))]))

(test (interp (parse '{with {x 5} {with {x 5} {+ x x}}}) (list (parse-func '{deffun {f x} 1}))) 10)
(test (interp (parse '{with {x 5} {with {y 5} {+ x y}}}) (list (parse-func '{deffun {f x} 1}))) 10)
(test (interp (parse '{with {x 5} {with {y 5} {with {z 5} {with {k 1} {+ x y}}}}}) (list (parse-func '{deffun {f x} 1}))) 10)
(test (interp (parse '{with {x 5} {with {y 5} {with {z 5} {with {k {with {y 1} {+ x y}}} {+ k 10}}}}}) (list (parse-func '{deffun {f x} 1}))) 16)
(test (interp (parse '{f 5}) (list (parse-func '{deffun {f x} {+ x x}}))) 10)
(test/exn (interp (parse '{with {x 5} {with {x 5} {+ x y}}}) (list (parse-func '{deffun {f x} 1}))) "No free identifier")