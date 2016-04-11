#lang plai
;; dollar->won: number -> number
;; convert dollar to won according to conversion rate 1100 
(define (dollar->won dollar) 
  (if (number? dollar)
      (* 1100 dollar)
      (error "Only number please")))

(test (dollar->won 100) 110000)
(test (dollar->won 0) 0)
(test (dollar->won -1) -1100)
(test/exn (dollar->won #t) "Only number please")

;; volume-cuboid: integer integer integer -> integer
;; calculate volume of cube
(define (volume-cuboid a b c)
  (if (and (and (integer? a) (integer? b)) (integer? c))
      (* a b c)
      (error "Only integer please")))
  

(test (volume-cuboid 1 1 1) 1)
(test (volume-cuboid 2 4 1) 8)
(test (volume-cuboid 1 5 5) 25)
(test (volume-cuboid -1 0 100) 0)
(test (volume-cuboid -2 -4 100) 800)
(test (volume-cuboid -2 -4 -8) -64)
(test/exn (volume-cuboid -1.5 0 2) "Only integer please")

;; is-even?: integer -> boolean
;; check whether integer is even
(define (is-even? number)
  (if (integer? number)
      (= (remainder number 2) 0)
      (error "Only integer please")))

(test (is-even? 2) #t)
(test (is-even? 1) #f)
(test (is-even? 0) #t)
(test (is-even? -1) #f)
(test (is-even? -2) #t)
(test/exn (is-even? -1.5) "Only integer please")

;; gcd: integer integer-> integer
;; get gcd of two integers
(define (gcd a b)
  (define c (max (abs a) (abs b)))
  (define d (min (abs a) (abs b)))
  (if (and (integer? c) (integer? d))
      (if (or (= c 0) (= d 0))
          c
          (gcd (remainder c d) d))
      (error "Only integer please")))
      

(test (gcd 0 1) 1)
(test (gcd 1 0) 1)
(test (gcd 1 1) 1)
(test (gcd 1 100) 1)
(test (gcd 3 5) 1)
(test (gcd 100 100) 100)
(test (gcd 500 100) 100)
(test (gcd 0 0) 0)
(test (gcd -2 -4) 2)
(test (gcd -100 100) 100)
(test/exn (gcd -99.9 1) "Only integer please")

;; lcm: integer integer -> integer
;; get lcm of two integers
(define (lcm a b)
  (define c (abs a))
  (define d (abs b))
  (if (and (integer? c) (integer? d))
      (if (or (= c 0) (= d 0))
          0
          (* (gcd c d) (/ c (gcd c d)) (/ d (gcd c d))))
      (error "Only integer please")))

(test (lcm 2 4) 4)
(test (lcm 4 4) 4)
(test (lcm 0 1) 0)
(test (lcm 0 0) 0)
(test (lcm 1 0) 0)
(test (lcm -1 0) 0)
(test (lcm -100 10) 100)
(test/exn (lcm 0.5 2.5) "Only integer please")
