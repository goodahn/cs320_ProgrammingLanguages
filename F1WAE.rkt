#lang plai
;; at first i can't think need of parse-fd, function list is empty

;; first order function
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?)
        (name-expr F1WAE?)
        (body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?) (arg F1WAE?)])

;; function
(define-type fundef
  [defun (fname symbol?) (farg symbol?) (body F1WAE?)])

;; parse : sexp -> F1WAE
;; convert s-expression into F1WAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(and (= (length sexp) 3) (symbol=? (first sexp) '+)) 
     (add (parse (second sexp)) (parse (third sexp)))]
    [(and (= (length sexp) 3) (symbol=? (first sexp) '-)) 
     (sub (parse (second sexp)) (parse (third sexp)))]
    [(and (= (length sexp) 3) (symbol=? (first sexp) 'with))
     (with 
      (first (second sexp)) (parse (second (second sexp))) 
      (parse (third sexp)))]
    [(and (= (length sexp) 3) (symbol=? (first sexp) 'app))
     (app (second sexp) (parse (third sexp)))]))

;;parse-fd : sexp -> fundef
(define (parse-fd sexp)
  (match sexp
      [(list 'defun fname farg body) (defun fname farg (parse body))]))


;; subst : F1WAE symbol F1WAE -> F1WAE
;; convert symbol which is in body into expr
(define (subst body x expr)
  (type-case F1WAE body
    [num (n) (num n)]
    [id (s) (if (symbol=? s x)
                (num expr)
                body)]
    [add (l r) (add (subst l x expr) (subst r x expr))]
    [sub (l r) (sub (subst l x expr) (subst r x expr))]
    [with (x_ expr_ body_) (with x_ (subst expr_ x expr) 
                                 (if (symbol=? x x_)
                                     body_
                                     (subst body_ x expr)))]
    [app (f a) (app (f (subst a x expr)))]))
;; lookup-fun : symbol list-of-fundef -> fundef 
(define (lookup-fun fname funcs) 
  (cond
    [(empty? funcs) (error "unknown function")]
    [(symbol=? fname (defun-fname (first funcs))) (first funcs)]
    [else (lookup-fun fname (rest funcs))]))
  
;; interp : F1WAE -> number
;; interprete F1WAE
(define (interp f1wae funcs)
  (type-case F1WAE f1wae
    [num (n) n]
    [id (s) (error "free variable")]
    [add (l r) (+ (interp l funcs) (interp r funcs))]
    [sub (l r) (- (interp l funcs) (interp r funcs))]
    [with (x expr body) (interp (subst body x (interp expr funcs)) funcs)]
    [app (fname fargs) (local [(define func-find (lookup-fun fname funcs))]
         (interp (subst (defun-body func-find) (defun-farg func-find) (interp fargs)) funcs))]))

(test (interp (parse '{with {x 5} {with {x 5} {+ x x}}}) (parse-fd '{defun f x 1})) 10)
;(test (interp (parse '{with {x 5} {with {y 5} {+ x y}}})) 10)
;(test (interp (parse '{with {x 5} {with {y 5} {with {z 5} {with {k 1} {+ x y}}}}})) 10)
;(test (interp (parse '{with {x 5} {with {y 5} {with {z 5} {with {k {with {y 1} {+ x y}}} {+ k 10}}}}})) 16)
;(test/exn (interp (parse '{with {x 5} {with {x 5} {+ x y}}})) "No free variable")