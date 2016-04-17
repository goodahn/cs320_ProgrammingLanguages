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
  [exprV (expr LFAE?) (ds DfrdSub?) (value (box/c (or/c false LFAEV?)))])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (value LFAEV?) (rest DfrdSub?)])

;; parse : sexp -> LFAE
;; convert s-expression into LFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun param body) (fun (first param) (parse body))]
    [(list fun arg) (app (parse fun) (parse arg))]))

;; lookup-ds : symbol DfrdSub -> LFAEV
(define (lookup-ds s ds)
  (type-case DfrdSub ds
    [mtSub () (error "free identifier")]
    [aSub (x value ds_)  (if (symbol=? s x)
                             value
                             (lookup-ds s ds_))]))
;; strict : LFAEV -> LFAEV
(define (strict lfaev)
  (type-case LFAEV lfaev
    [exprV (expr ds value) (if (not (unbox value))
                               (local [(define v (strict (interp expr ds)))]
                                 (begin (set-box! value v)
                                        v))
                               (unbox value))]
    [else lfaev]))

;; num-op : op -> process
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n (strict x)) (numV-n (strict y))))))

(define num+ (num-op +))
(define num- (num-op -))

;; interp : LFAE ds -> LFAEV
(define (interp lfae ds)
  (type-case LFAE lfae
    [num (n) (numV n)]
    [id (s) (lookup-ds s ds)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [fun (x body) (closureV x body ds)]
    [app (func farg) (local [(define fval (strict (interp func ds)))
                             (define aval (exprV farg ds (box #f)))]
                       (interp (closureV-body fval)
                               (aSub (closureV-param fval)
                                     aval
                                     (closureV-ds fval))))]))
(test (interp (parse '{{fun {x} {+ x 1}} 1}) (mtSub)) (numV 2))