#lang plai

(define-type FWAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (name-expr FWAE?) (body FWAE?)]
  [fun (param symbol?) (body FWAE?)]
  [app (func FWAE?) (arg FWAE?)])

(define-type FWAEV
  [numV (n number?)]
  [closureV (param symbol?) (body FWAE?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list x expr) body) (with x (parse expr) (parse body))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list f a) (app (parse f) (parse a))]))
  
(define (subst body x expr)
  (type-case FWAE body
    [num (n) body]
    [id (s) (if (symbol=? s x)
                expr
                body)]
    [add (l r) (add (subst l x expr) (subst r x expr))]
    [sub (l r) (sub (subst l x expr) (subst r x expr))]
    [with (x_ expr_ body_) (with x_ (subst expr_ x expr) (if (symbol=? x x_)
                                                             body_
                                                             (subst body_ x expr)))]
    [fun (p body) (fun x (subst body p expr))]
    [app (f a) (app f (subst a x expr))]))

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define (interp fwae)
  (type-case FWAE fwae
    [num (n) (numV n)]
    [id (s) (error "No free identifier")]
    [add (l r) (num+ (interp l) (interp r))]
    [sub (l r) (num- (interp l) (interp r))]
    [with (x expr body) (interp (subst body x (interp expr)))]
    [fun (x body) (closureV x body)]
    [app (f a) (local [(define fval (interp f))]
                 (interp (subst (closureV-body fval) 
                                (closureV-param fval) 
                                a)))]))

(test (interp (parse '{{fun {x} {+ x x}} 5})) (numV 10))