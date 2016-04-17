#lang plai

(define-type LFAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs LFAE?) (rhs LFAE?)]
  [sub (lhs LFAE?) (rhs LFAE?)]
  [fun (param symbol?) (body LFAE?)]
  [app (func LFAE?) (arg LFAE?)])

(define-type LFAEV
  [numV (n number?)]
  [closureV (param symbol?) (body LFAE?) (ds DfrdSub?)]
  [exprV (expr LFAE?) (ds DfrdSub?)])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (value LFAEV?) (rest DfrdSub?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list f a) (app (parse f) (parse a))]))
  
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n (strict x)) (numV-n (strict y))))))

(define num+ (num-op +))
(define num- (num-op -))

(define (lookup-ds s ds)
  (type-case DfrdSub ds
    [mtSub () (error "no free identifier")]
    [aSub (x body rest) (if (symbol=? x s)
                            body
                            (lookup-ds s rest))]))

(define (strict x)
  (type-case LFAEV x
    [exprV (value ds_) (strict (interp value ds_))]
    [else x]))

(define (interp lfae ds)
  (type-case LFAE lfae
    [num (n) (numV n)]
    [id (s) (lookup-ds s ds)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [fun (x body) (closureV x body ds)]
    [app (f a) (local [(define fval (interp f ds))]
                 (interp (closureV-body fval) 
                         (aSub (closureV-param fval) 
                               (exprV a ds) 
                               (closureV-ds fval))))]))

(test (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub)) (numV 10))