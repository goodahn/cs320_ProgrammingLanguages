#lang plai
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

;; parse : sexp -> F1WAE
;; convert s-expression into F1WAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(and (= (length sexp) 3) (symbol=? (first sexp) 'add)) 
     (add (parse (second sexp)) (parse (third sexp)))]
    [(and (= (length sexp) 3) (symbol=? (first sexp) 'sub)) 
     (sub (parse (second sexp)) (parse (third sexp)))]
    [(and (= (length sexp) 3) (symbol=? (first sexp) 'with))
     (with 
      (list (second (first sexp)) (parse (second (second sexp)))) 
      (parse (third sexp)))]
    [(and (= (length sexp) 3) (symbol=? (first sexp) 'app))
     (app (second sexp) (parse (third sexp)))]))

;; function
(define-type fundef
  [defun (fname symbol?) (body F1WAE?)])

;; subst : F1WAE symbol F1WAE -> F1WAE
;; convert symbol which is in body into expr
(define (subst body x expr)
  (type-case F1WAE body
    [num (n) (num n)]
    [id (s) (if (symbol=? s x)
                expr
                body)]
    [add (l r) (add (subst l x expr) (subst r x expr))]
    [sub (l r) (sub (subst l x expr) (subst r x expr))]
    [with (x_ expr_ body_) (with x_ expr_ (if (symbol=? x x_)
                                              body_
                                              (subst body_ x expr)))]
    [app (f a) (app (f (subst a x expr)))]))
  
;; interp : F1WAE -> number
;; interprete F1WAE
(define (interp f1wae funcs)
  (type-case F1WAE f1wae
    [num (n) n]
    [id (s) (error "free variable")]
    [add (l r) (+ (interp l funcs) (interp r funcs))]
    [sub (l r) (- (interp arg l) (interp arg r))]
    [with (x expr body) (interp (subst body x (interp expr)))]
    [app (fname fargs) (lookup-fun fname)]))