#lang plai

(define-type RCFAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (func RCFAE?) (arg RCFAE?)]
  [rec (fname symbol?) (name-expr RCFAE?) (body RCFAE?)]
  [if0 (test-expr RCFAE?) (then-expr RCFAE?) (else-expr RCFAE?)])
  
(define-type RCFAEV
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (ds DfrdSub?)])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (value RCFAEV?) (rest DfrdSub?)]
  [recSub (name symbol?) (body (box/c RCFAEV?)) (rest DfrdSub?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list 'rec (list fname f-expr) body) (rec fname (parse f-expr) (parse body))]
    [(list 'if0 test-exp then-exp else-exp) (if0 (parse test-exp) (parse then-exp) (parse else-exp))]
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
                            (lookup-ds s rest))]
    [recSub (f body rest) (if (symbol=? s f)
                              (unbox body)
                              (lookup-ds s rest))]))

(define (interp rcfae ds)
  (type-case RCFAE rcfae
    [num (n) (numV n)]
    [id (s) (lookup-ds s ds)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [fun (name body) (closureV name body ds)]
    [app (f a) (local [(define fval (interp f ds))]
                 (interp (closureV-body fval) 
                         (aSub (closureV-param fval) 
                               (interp a ds) 
                               (closureV-ds fval))))]
    [rec (f f-expr body) (local [(define v (box (numV 42)))
                          (define new-ds (recSub f v ds))]
                    (begin (set-box! v (interp f-expr new-ds))
                           (interp body new-ds)))]
    [if0 (t-expr then-expr else-expr) (if (= 0 (numV-n (interp t-expr ds)))
                                          (interp then-expr ds)
                                          (interp else-expr ds))]))

(test (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub)) (numV 10))
(test (interp (parse '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}}
{count 8}}) (mtSub)) (numV 8))