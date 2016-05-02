#lang plai
(define total 0)

(define (a)
  `(("Current total" ,total)
    "call a2 to add 2"
    "call a3 to add 3"))

(define (a2)
  (begin (set! total (+ total 2))
         (a)))

(define (a3)
  (begin (set! total (+ total 3))
         (a)))

(test (a) '(("Current total" 0) "call a2 to add 2" "call a3 to add 3"))
(test (a2) '(("Current total" 2) "call a2 to add 2" "call a3 to add 3"))
(test (a3) '(("Current total" 5) "call a2 to add 2" "call a3 to add 3"))
