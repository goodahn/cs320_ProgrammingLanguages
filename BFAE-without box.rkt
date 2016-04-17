#lang plai

(define-type BFAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs BFAE?) (rhs BFAE?)]
  [sub (lhs BFAE?) (rhs BFAE?)]
  [fun (param symbol?) (body BFAE?)]
  [app (func BFAE?) (arg BFAE?)]
  [newbox (value BFAE?)]
  [setbox (address BFAE?) (value BFAE?)]
  [openbox (address BFAE?)]
  [seqn (expr1 BFAE?) (expr2 BFAE?)])

(define-type BFAEV
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (rest DfrdSub?)]
  [boxV (address integer?)])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (body BFAE?) (rest DfrdSub?)])

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BFAEV?) (rest Store?)])

(define-type Value*Store
  [v*s (value BFAEV?) (store Store?)])

(define (lookup-ds s ds)
  (type-case DfrdSub ds
    [mtSub () (error "No free identifier")]
    [aSub (x body rest) (if (symbol=? x s)
                            body
                            (lookup-ds s rest))]))

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun x body) (fun (first x) (parse body))]
    [(list 'newbox value) (newbox (parse value))]
    [(list 'setbox t v) (setbox (parse t) (parse v))]
    [(list 'openbox t) (openbox (parse t))]
    [(list 'seqn f l) (seqn (parse f) (parse l))]
    [(list f a) (app (parse f) (parse a))]))

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define (interp-two exp1 exp2 ds st handle)
  (type-case Value*Store (interp exp1 ds st)
    [v*s (v1 st1)
         (type-case Value*Store (interp exp2 ds st1)
           [v*s (v2 st2)
                (handle v1 v2 st2)])]))
(define (max-addr st)
  (type-case Store st
    [mtSto () -1]
    [aSto (addr value rest)
          (max addr (max-addr rest))]))

(define (malloc st)
  (+ 1 (max-addr st)))

(define (lookup-st addr st)
  (type-case Store st
    [mtSto () (error "no allocated memory")]
    [aSto (a v st1)
          (if (= addr a)
              v
              (lookup-st addr st1))]))

(define (interp bfae ds st)
  (type-case BFAE bfae
    [num (n) (numV n)]
    [id (s) (lookup-ds s ds)]
    [add (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1)
                             (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1)
                             (v*s (num- v1 v2) st1)))]
    [fun (x body) (closureV x body ds)]
    [app (f a) (interp-two f a ds st 
                           (lambda (v1 v2 st1)
                             (interp (closureV-body v1)
                                          (aSub (closureV-param v1)
                                                v2
                                                (closureV-rest v1))
                                          st1)))]      
    [newbox (v) (type-case Value*Store (interp v ds st)
                    [v*s (v1 st1) 
                         (local [(define addr (malloc st1))]
                           (v*s (boxV addr)
                                (aSto addr v1 st1)))])]
    [setbox (t v) (interp-two t v ds st
                              (lambda (v1 v2 st1)
                                (v*s v2
                                     (aSto (boxV-address v1)
                                           v2
                                           st1))))]
    [openbox (t) (type-case Value*Store (interp t ds st)
                   [v*s (v1 st1)
                        (v*s (lookup-st (boxV-address t) st1) st1)])]
    [seqn (exp1 exp2) (interp-two exp1 exp2 ds st
                                  (lambda (v1 v2 st1)
                                    (v*s v2 st1)))]))