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
  [closureV (param symbol?) (body FWAE?) (ds DfrdSub?)])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (value FWAEV?) (rest DfrdSub?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list x expr) body) (with x (parse expr) (parse body))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list f a) (app (parse f) (parse a))]))
  
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define (lookup-ds s ds)
  (type-case DfrdSub ds
    [mtSub () (error "no free identifier")]
    [aSub (x body rest) (if (symbol=? x s)
                            body
                            (lookup-ds s rest))]))

(define (interp fwae ds)
  (type-case FWAE fwae
    [num (n) (numV n)]
    [id (s) (lookup-ds s ds)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [with (x expr body) (interp body (aSub x expr ds))]
    [fun (x body) (closureV x body ds)]
    [app (f a) (local [(define fval (interp f ds))]
                 (interp (closureV-body fval) 
                         (aSub (closureV-param fval) 
                               (interp a ds) 
                               (closureV-ds fval))))]))

(test (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub)) (numV 10))
(interp (parse '{{fun {a} {{fun {b} a} 10}} 10}) (mtSub))