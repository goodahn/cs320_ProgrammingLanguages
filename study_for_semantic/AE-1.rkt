#lang plai
(define-type AE
[num (n number?)]
[add (lhs AE?) 
(rhs AE?)]
[sub (lhs AE?) 
(rhs AE?)])

;; parse : sexp -> AE
;; convert s-expression into AE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and (= 3 (length sexp)) (symbol=? (first sexp) '+))
     (add (parse (second sexp)) (parse (third sexp)))]
    [(and (= 3 (length sexp)) (symbol=? (first sexp) '-))
     (sub (parse (second sexp)) (parse (third sexp)))]
    [else (error "Wrong format")]))
(test (parse 5) (num 5))
(test (parse '{+ 5 4}) (add (num 5) (num 4)))
(test (parse '{- 5 4}) (sub (num 5) (num 4)))

;; interp : ae -> num
;; interprete AE
(define (interp ae)
  (type-case AE ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]))
(test (interp (parse 5)) 5)
(test (interp (parse '{+ 5 4})) 9)
(test (interp (parse '{- 5 4})) 1)

