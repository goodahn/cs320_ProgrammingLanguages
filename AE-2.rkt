#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)]
  [sub (lhs AE?) (rhs AE?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]))

(define (interp ae)
  (type-case AE ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]))

(test (interp (parse '5)) 5)
(test (interp (parse '-1)) -1)
(test (interp (parse '{+ 1 5})) 6)
(test (interp (parse '{+ 1 -5})) -4)
(test (interp (parse '{+ 1 {- 0 5}})) -4)