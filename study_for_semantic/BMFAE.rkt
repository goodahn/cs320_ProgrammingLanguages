#lang plai

(define-type BMFAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs BMFAE?) (rhs BMFAE?)]
  [sub (lhs BMFAE?) (rhs BMFAE?)]
  [fun (param symbol?) (body BMFAE?)]
  [app (func BMFAE?) (arg BMFAE?)]
  [newbox (value BMFAE?)]
  [setbox (target BMFAE?) (value BMFAE?)]
  [openbox (target BMFAE?)]
  [seqn (expr1 BMFAE?) (expr2 BMFAE?)]
  [setid (target symbol?) (value BMFAE?)])

(define-type DfrdSub
  [mtSub]
  [aSub (param symbol?) (address integer?) (rest DfrdSub?)])

(define-type Store
  [mtSto]
  [aSto (address number?) (value BMFAEV?) (rest Store?)])

(define-type Value*Store
  [v*s (value BMFAEV?) (store Store?)])

(define-type BMFAEV
  [numV (n number?)]
  [closureV (param symbol?) (body BMFAE?) (ds DfrdSub?)]
  [boxV (address integer?)])

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
    [(list 'seqn ex1 ex2) (seqn (parse ex1) (parse ex2))]
    [(list 'setid t v) (setid (parse t) (parse v))]
    [(list f a) (app (parse f) (parse a))]))

(define (lookup-ds s ds)
  (type-case DfrdSub ds
    [mtSub () (error "free identifier")]
    [aSub (x addr rest) (if (symbol=? s x)
                            addr
                            (lookup-ds s rest))]))

(define (lookup-st a st)
  (type-case Store st
    [mtSto () (error "not allocated address")]
    [aSto (addr v rest) (if (= a addr)
                            v
                            (lookup-st a rest))]))

(define (num-op op)
  (lambda (x y) (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define (interp-two ex1 ex2 ds st handle)
  (type-case Value*Store (interp ex1 ds st)
    [v*s (v1 st1) (type-case Value*Store (interp ex2 ds st1)
                    [v*s (v2 st2) (handle v1 v2 st2)])]))

(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (a v rest) (+ 1 (max-address rest))]))

(define (malloc st)
  (+ 1 (max-address st)))

(define (interp bmfae ds st)
  (type-case BMFAE bmfae
    [num (n) (v*s (numV n) st)]
    [id (s) (v*s (lookup-st (lookup-ds s ds) st) st)]
    [add (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1)
                             (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1)
                             (v*s (num- v1 v2) st1)))]
    [fun (x body) (v*s (closureV x body ds) st)]
    [app (f a) (interp-two f a ds st (lambda (v1 v2 st1)
                                       (interp (closureV-body v1)
                                               (aSub (closureV-param v1)
                                                     (malloc st1)
                                                     ds)
                                               (aSto (malloc st1)
                                                     v2
                                                     st1))))]
    [newbox (v) (local [(define new-addr (malloc st))]
                  (type-case Value*Store (interp v ds st)
                    [v*s (v1 st1) (v*s (boxV new-addr) (aSto new-addr v1 st1))]))]
    [setbox (t v) (interp-two t v ds st (lambda (v1 v2 st1)
                                          (v*s v2 (aSto (boxV-address v1) v2 st1))))]
    [openbox (t) (type-case Value*Store (interp t ds st)
                   [v*s (v1 st1) (v*s (lookup-st (boxV-address v1) st1)
                                      st1)])]
    [seqn (ex1 ex2) (interp-two ex1 ex2 ds st (lambda (v1 v2 st1)
                                                (v*s v2 st1)))]
    [setid (s v) (type-case Value*Store (interp v ds st)
                   [v*s (v1 st1) (v*s v1
                                      (aSto (lookup-ds s ds) v1 st1))])]))
                  
(test (interp (parse '1) (mtSub) (mtSto)) (v*s (numV 1) (mtSto)))
(test (interp (parse '{fun {x} {+ x 2}}) (mtSub) (mtSto)) (v*s (closureV 'x (add (id 'x) (num 2)) (mtSub)) (mtSto)))
(test (interp (parse '{{fun {x} {+ x 2}} 2}) (mtSub) (mtSto)) (v*s (numV 4) (aSto 1 (numV 2) (mtSto))))
(test (interp (parse '{newbox 1}) (mtSub) (mtSto)) (v*s (boxV 1) (aSto 1 (numV 1) (mtSto))))
(test (interp (parse '{setbox {newbox 1} 120}) (mtSub) (mtSto)) (v*s (numV 120) (aSto 1 (numV 120) (aSto 1 (numV 1) (mtSto)))))