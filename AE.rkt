#lang plai
;; AE is arithmetic expression
(define-type AE
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)]
  [num (n number?)])
;; parse: sexp -> AE
;; conver s-expressions into AE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(= 3 (length sexp)) 
     (if (eqv? (first sexp) '+)
         (add (parse (second sexp)) (parse (third sexp)))
         (if (eqv? (first sexp) '-)
             (sub (parse (second sexp)) (parse (third sexp)))
             (error "You should put right format")))]
    [else (error "You should put right format")]))
    
  
;; interp: AE -> number
;; interprete AE to number
(define (interp ae)
  (type-case AE ae
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [num (n) n]))

(test (interp (num 5)) 5)
(test (interp (add (num 5) (num 4))) 9)
(test (interp (sub (num 5) (num 4))) 1)
(test (interp (sub (add (num 5) (num 4)) (num 4))) 5)
(test (parse '5) (num 5))
(test (parse '-1) (num -1))
(test (parse '{+ 5 4}) (add (num 5) (num 4)))
(test (parse '{+ {- 5 4} {+ 5 5}}) (add (sub (num 5) (num 4)) (add (num 5) (num 5))))
(test (parse '{+ {+ {- 5 4} 5} 1}) (add (add (sub (num 5) (num 4)) (num 5)) (num 1)))