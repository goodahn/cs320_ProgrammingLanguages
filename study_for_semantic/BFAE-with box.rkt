#lang plai

(define-type BFAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs BFAE?) (rhs BFAE?)]
  [sub (lhs BFAE?) (rhs BFAE?)]
  [fun (param symbol?) (body BFAE?)]
  [app (func BFAE?) (arg BFAE?)]
  [newbox (value BFAE?)]
  [setbox (target BFAE?) (value BFAE?)]
  [openbox (target BFAE?)]
  [seqn (expr1 BFAE?) (expr2 BFAE?)])

(define-type BFAEV
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (rest DfrdSub?)]
  [boxV (value (box/c BFAEV?))])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (body BFAEV?) (rest DfrdSub?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list 'newbox v) (newbox (parse v))]
    [(list 'setbox t v) (setbox (parse t) (parse v))]
    [(list 'openbox t) (openbox (parse t))]
    [(list 'seqn expr1 expr2) (seqn (parse expr1) (parse expr2))]
    [(list f a) (app (parse f) (parse a))]))

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define (lookup-ds s ds)
  (type-case DfrdSub ds
    [mtSub () (error "free identifier")]
    [aSub (x body rest) (if (symbol=? s x)
                            body
                            (lookup-ds s rest))]))

(define (interp bfae ds)
  (type-case BFAE bfae
    [num (n) (numV n)]
    [id (s) (lookup-ds s ds)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [fun (x body) (closureV x body ds)]
    [app (f a) (local [(define fval (interp f ds))
                       (define aval (interp a ds))]
                 (interp (closureV-body fval)
                         (aSub (closureV-param fval)
                               aval
                               (closureV-rest fval))))]
    [newbox (v) (boxV (box (interp v ds)))]
    [setbox (t v) (set-box! (boxV-value (interp t ds)) (interp v ds))]
    [openbox (t) (unbox (boxV-value (interp t ds)))]
    [seqn (f l) (begin (interp f ds)
                       (interp l ds))]))

(test (interp (parse '{{fun {q} {seqn {setbox {seqn {setbox q 12} q} {openbox q}} {openbox q}}} {newbox 10}}) (mtSub)) (numV 12))