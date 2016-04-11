#lang plai

;; fwae
(define-type FWAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?)
        (name-expr FWAE?)
        (body FWAE?)]
  [fun (param symbol?) (body FWAE?)]
  [app (func FWAE?) (arg FWAE?)])

;; parse : sexp -> FWAE
;; convert s-expression into FWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with x expr body) (with x (parse expr) (parse body))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list func arg) (app (parse func) (parse arg))]))

;; fwae value
(define-type FWAE-V
  [numV (n number?)]
  [closureV (param symbol?) (body FWAE?)])

;(define (num-op op l r)
;  (op (numV-n l) (numV-n r)))
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y)))))

(define (num+ l r) (num-op + l r))
(define (num- l r) (num-op - l r))

;; subst : FWAE symbol FWAE -> FWAE
(define (subst body x expr)
  (type-case FWAE body
    [num (n) body]
    [id (s) (if (symbol=? body x)
            (num expr)
            body)]
    [add (l r) (num+ (subst l x expr) (subst r x expr))]
    [sub (l r) (num- (subst l x expr) (subst r x expr))]
    [with (x_ expr_ body_) (with x_ (subst expr_ x expr)
                                 (if (symbol=? x x_)
                                     body_
                                     (subst body_ x expr)))]
    [fun (x_ f_) (if (symbol=? x x_)
                     body
                     (fun id (subst f_ x expr)))]
    [app (f a) (app (subst f x expr)
                    (subst a x expr))]))

;; interp : FWAE -> FWAE
(define (interp fwae)
  (type-case FWAE fwae
    [num (n) fwae]
    [id (s) (error "free variable")]
    [add (l r) (num+ (interp l) (interp r))]
    [sub (l r) (num- (interp l) (interp r))]
    [with (x expr body) (interp (subst body x (interp expr)))]
    [fun (x f) fwae]
    [app (f a) (local [(define fval (interp f))]
                 (interp (subst (fun-body fval)
                                (fun-param fval)
                                (interp a))))]))