#lang plai

;; FWAE
(define-type FWAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [fun (param symbol?) (body FWAE?)]
  (app (func FWAE?) (arg FWAE?)))

;; Deferred Substitution
(define-type DefrdSub
  [mtSub]
  [aSub (param symbol?) (value FWAEV?) (rest DefrdSub?)])

;; FWAE value
(define-type FWAEV
  [numV (n number?)]
  [closureV (param symbol?) (body FWAE?) (ds DefrdSub?)])

;; parse : sexp -> FWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with x expr body) (with x (parse expr) (parse body))]
    [(list 'fun x f) (fun (first x) (parse f))]
    [(list f a) (app (parse f) (parse a))]))

;; lookup : symbol DfrdSub -> FWAE
;; find value of parama which is in environment
(define (lookup param ds)
  (if (symbol=? param (aSub-param ds))
      (aSub-value ds)
      (lookup param (aSub-rest ds))))

;; num-op : op -> lambda function
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))) 

(define num+ (num-op +))
(define num- (num-op -))
;; interp : FWAE DefrdSub -> number
;;          FWAE DefrdSub -> closureV
(define (interp fwae ds)
  (type-case FWAE fwae
    [num (n) (numV n)]
    [id (s) (lookup s ds)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [with (x expr body) (interp body (aSub x (interp expr) ds))]
    [fun (x body) (closureV x body ds)]
    [app (func arg) (local [(define fval (interp func ds))
                            (define aval (interp arg ds))]
                      (interp (closureV-body fval) 
                              (aSub (closureV-param fval) aval (closureV-ds fval))))]))

(test (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub)) (numV 10))