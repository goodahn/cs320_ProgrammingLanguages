#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")

;; rec unit
(define-type REC
 [rec (name symbol?) (content FnWAE?)])

;; FnWAE
(define-type FnWAE
 [num (n number?)]
 [id (name symbol?)]
 [add (lhs FnWAE?) (rhs FnWAE?)]
 [sub (lhs FnWAE?) (rhs FnWAE?)]
 [with (name symbol?) (name-expr FnWAE?) (body FnWAE?)]
 [app (fname symbol?) (arglist (listof FnWAE?))]
 [defrec (contents (listof REC?))]
 [get (content FnWAE?) (target symbol?)])

;; FnWAEV
(define-type FnWAEV
  [numV (n number?)]
  [recV (contents (listof REC?))])

;; FunDef
(define-type FunDef
[deffun (fname symbol?) (params (listof symbol?)) (body FnWAE?)])

;; uniq? : list-of-symbol -> bool
(define (uniq? x)
 (if (= (length x) (length (remove-duplicates x)))
     #t
     #f))

;; parse-sexp : sexp -> FnWAE
;; convert s-expression into FnWAE
(define (parse-sexpr sexp)
(match sexp
  [(? number?) (num sexp)]
  [(? symbol?) (id sexp)]
  [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
  [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
  [(list 'with (list x expr) body) (with x (parse-sexpr expr) (parse-sexpr body))]
  [(list 'deffun x body) (deffun x (parse-sexpr body))]
  [(list 'rec rec_ ...) (if (uniq? (map (lambda (x) (first x)) rec_))
                            (defrec (map (lambda (x) (rec (first x) (parse-sexpr (second x)))) rec_))
                            (error "duplicate fields"))]
  [(list 'get content target) (get (parse-sexpr content) target)]
  [(list fname arglist ...) (app fname (map parse-sexpr arglist))]
  [else (error 'parse "bad syntax")]))

;; parses a string containing a WAE expression to a WAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; parse-defn : sexp -> FunDef
;; convert s-expression into FunDef
(define (parse-defn sexp)
 (match sexp
   [(list 'deffun (list f x ...) body)
    (unless (uniq? x)
      (error 'parse-defn "bad syntax"))
    (deffun f x (parse-sexpr body))]))

;; lookup-param : symbol list-of-FnWAE -> FnWAE
(define (lookup-param-pos x arglist pos)
 (if (empty? arglist)
     (error "free variable")
     (if (symbol=? x (id-name (first arglist)))
         pos
         (lookup-param-pos x (rest arglist) (+ pos 1)))))

;; lookup-fundef : symbol list-of-FunDef -> FunDef
(define (lookup-fundef ftn flist)
 (if (empty? flist)
     -1
     (if (symbol=? ftn (deffun-fname (first flist)))
         (first flist)
         (lookup-fundef ftn (rest flist)))))
 
;; lookup-rec : FnWAE symbol -> FnWAE
(define (lookup-rec contents target)
  (if (empty? (defrec-contents contents))
      (error "no such field")
      (if (symbol=? (rec-name (first (defrec-contents contents))) target)
          (rec-content (first (defrec-contents contents)))
          (lookup-rec (defrec (rest (defrec-contents contents))) target))))

;; subst : FnWAE symbol FnWAE -> FnWAE
(define (subst body x expr)
 (type-case FnWAE body
   [num (n) body]
   [id (s) (if (symbol=? x s)
               expr
               body)]
   [add (l r) (add (subst l x expr) (subst r x expr))]
   [sub (l r) (sub (subst l x expr) (subst r x expr))]
   [with (x_ expr_ body_) (with x_ (subst expr_ x expr) (if (symbol=? x x_)
                                                            body_
                                                            (subst body_ x expr)))]
   #|[app (f a) (local [(define tpos (lookup-param x a 0))]
                (if (not (= tpos -1))
                    (app f (append (drop-right a 
                                               (- (length a) 
                                                  tpos)) 
                                   (list expr)
                                   (rest (drop a tpos))))
                    body))]|#
   [app (f a) (app f (map (lambda (y) (subst y x expr)) a))]
   [defrec (contents) (defrec (map (lambda (y) (rec (rec-name y) (subst (rec-content y) x expr))) contents))]
   [get (content target) (get (subst content x expr) target)])) 

;; subst-fun : FnWAE lisfof-symbol listof-FnWAE -> FnWAE
(define (subst-fun body xlist arglist)
 (if (empty? arglist)
     body
     (subst-fun (subst body
                       (first xlist)
                       (first arglist))
                (rest xlist)
                (rest arglist))))

(define (num-op op)
  (lambda (x y) (num (op (num-n x) (num-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

;; interp : FnWAE list-of-FunDef -> number or record
(define (interp fnwae funcs)
 (type-case FnWAE fnwae
   [num (n) (num n)]
   [id (s) (error "no free variable")]
   [add (l r) (num+ (interp l funcs)
                 (interp r funcs))]
   [sub (l r) (num- (interp l funcs)
                 (interp r funcs))]
   [with (x expr body) (interp (subst body
                                      x
                                      (interp expr funcs))
                               funcs)]
   [app (f a) (local [(define fval (lookup-fundef f funcs))]                 
                (if (= (length (deffun-params fval))
                       (length a))
                    (interp (subst-fun (deffun-body fval)
                                       (deffun-params fval)
                                       a)
                            funcs)
                    (error "wrong arity")))]
   [defrec (content) (defrec (map (lambda (y) (rec (rec-name y) (interp (rec-content y) funcs))) content))]
   [get (contents target) (interp (lookup-rec (interp contents funcs) target) funcs)]))

;; interp-FnWAEV : FnWAEV -> number or 'record
(define (interp-FnWAEV fnwaev)
  (if (num? fnwaev)
      (num-n fnwaev)
      (if (defrec? fnwaev)
          'record
          (error "no such type"))))
;; run : string listof-sexp -> number or symbol
(define (run str funcs)
  (interp-FnWAEV (interp (parse str) funcs)))

(test (run "{rec {a 10} {b {+ 1 2}}}" empty)
      'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty)
      3)
(test/exn (run "{get {rec {b 10} {b {+ 1 2}}} b}" empty)
          "duplicate fields")
(test/exn (run "{get {rec {a 10}} b}" empty)
          "no such field")
(test (run "{g {rec {a 0} {c 12} {b 7}}}"
           (list (parse-defn '{deffun {g r} {get r c}})))
      12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty)
      'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty)
      0)
(test/exn (run "{rec {z {get {rec {z 0}} y}}}" empty)
          "no such field")
(test (run "{with {x {f 2 5}} {g x}}" (list (parse-defn '{deffun {f a b} {+ a b}}) (parse-defn '{deffun {g x} {- x 5}}))) 2)
(test (run "{f 1 2}" (list (parse-defn '{deffun {f x y} {+ x y}}))) 3)
(test (run "{+ {f} {f}}" (list (parse-defn '{deffun {f} 5}))) 10)
(test (run "{h 1 4 5 6}" (list (parse-defn '{deffun {h x y z w} {+ x w}}) (parse-defn '{deffun {g x y z w} {+ y z}}))) 7)
(test (run "{with {x 10} {- {+ x {f}} {g 4}}}" (list (parse-defn '{deffun {f} 4}) (parse-defn '{deffun {g x} {+ x x}}))) 6)

(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{with {x 3} {with {y 5} {get {rec {a x} {b y}} a}}}" empty) 3)
(test (run "{with {x {f {rec {a 10} {b 5}} 2}} {g x}}" (list (parse-defn '{deffun {f a b} {+ {get a a} b}}) (parse-defn '{deffun {g x} {+ 5 x}}))) 17)
(test (run "{get {f 1 2 3 4 5} c}" (list (parse-defn '{deffun {f a b c d e} {rec {a a} {b b} {c c} {d d} {e e}}}))) 3)
(test (run "{get {f 1 2 3} b}" (list (parse-defn '{deffun {f a b c} {rec {a a} {b b} {c c}}}))) 2)
(test (run "{get {f 1 2 3} y}" (list (parse-defn '{deffun {f a b c} {rec {x a} {y b} {z c} {d 2} {e 3}}}))) 2)
(test (run "{get {f 1 2 3} d}" (list (parse-defn '{deffun {f a b c} {rec {x a} {y b} {z c} {d 2} {e 3}}}))) 2)
(test (run "{f {get {get {rec {a {rec {a 10} {b {- 5 2}}}} {b {get {rec {x 50}} x}}} a} b}}" (list (parse-defn '{deffun {f x} {+ 5 x}}))) 8)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{rec {a 10}}" empty) `record)
(test (run "{get {rec {a 10}} a}" empty) 10)
(test (run "{get {rec {a {+ 1 2}}} a}" empty) 3)
(test (run "{rec }" empty) `record)
(test (run "{get {rec {a {rec {b 10}}}} a}" empty) `record)
(test (run "{get {get {rec {a {rec {a 10}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} b}" empty) 20)
(test (run "{+ {get {rec {a 10}} a} {get {rec {a 20}} a}}" empty) 30)
(test (run "{+ {get {rec {a 10}} a} {get {rec {a 20}} a}}" empty) 30)
(test (run "{rec {a 10}}" empty) `record)
(test (run "{rec {a {- 2 1}}}" empty) `record)
(test (run "{get {rec {a 10}} a}" empty) 10)
(test (run "{get {rec {a {- 2 1}}} a}" empty) 1)
(test (run "{get {rec {a {rec {b 10}}}} a}" empty) `record)
(test (run "{get {get {rec {a {rec {a 10}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} b}" empty) 20)
(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{with {y {rec {x 1} {y 2} {z 3}}} {get y y}}" empty) 2)
(test (run "{with {y {rec {x 1} {y 2} {z 3}}} {get y z}}" empty) 3)
(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)