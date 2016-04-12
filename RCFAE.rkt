#lang plai

(define-type RCFAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (func RCFAE?) (arg RCFAE?)]
  [rec (name symbol?) (name-expr RCFAE?) (body RCFAE?)]
  [if0 (test-expr RCFAE?) (then-expr RCFAE?) (else-expr RCFAE?)])

(define-type RCFAEV
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (ds DfrdSub?)])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (body RCFAEV?) (rest DfrdSub?)]
  [recSub (name symbol?) (value (box/c RCFAEV?)) (rest DfrdSub?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'rec (list x expr) body) (rec x (parse expr) (parse body))]
    [(list 'if0 test-expr then-expr else-expr) (if0 (parse test-expr) (parse then-expr) (parse else-expr))]))

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define (lookup-ds s ds)
  (type-case DfrdSub ds
    [mtSub () (error "free identifier")]
    [aSub (x body rest) (if (symbol=? x s)
                            body
                            (lookup-ds s rest))]
    [recSub (x v rest) (if (symbol=? x s)
                           (unbox v)
                           (lookup-ds s rest))]))

(define (interp rcfae ds)
  (type-case RCFAE rcfae
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
                               ds)))]
    [rec (name name-expr body) (local [(define v (box (numV 42)))
                                       (define new-ds (recSub name v ds))]
                                 (begin (set-box! v (interp name-expr ds))
                                   (interp body new-ds)))]
    [if0 (test-expr then-expr else-expr) (if (= 0 (numV-n (interp test-expr ds)))
                                             (interp then-expr ds)
                                             (interp else-expr ds))]))

(test (interp (parse '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}) (mtSub)) (numV 8))