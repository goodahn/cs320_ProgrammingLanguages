#lang plai

(define total 0)

(define (b)
  `(("Current total:" ,total)
    "Call b2 to add 2 with total"
    "Call b3 to add 3 with total"))

(define (b2 val)
  (begin (set! total (+ 2 val))
         (b)))

(define (b3 val)
  (begin (set! total (+ 3 val))
         (b)))

(test (b) '(("Current total:" 0) "Call b2 to add 2 with total" "Call b3 to add 3 with total"))
(test (b2 0) '(("Current total:" 2) "Call b2 to add 2 with total" "Call b3 to add 3 with total"))
(test (b3 2) '(("Current total:" 5) "Call b2 to add 2 with total" "Call b3 to add 3 with total"))
