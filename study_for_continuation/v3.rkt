#lang plai

(define total empty)

(define (remember val)
  (local [(define key (length total))]
    (begin (set! total (append total (list val)))
           key)))

(define (c)
  (do-c "*"))

(define  (do-c val)
  (local [(define key (remember val))]
    `(("Current value:" ,val)
      ("call c2 to add hello with" ,key)
      ("call c3 to add bye with" ,key))))

(define (c2 index)
  (local [(define val (list-ref total index))]
    (do-c (string-append val "hello"))))

(define (c3 index)
  (local [(define val (list-ref total index))]
    (do-c (string-append val "bye"))))

(test (c) '(("Current value:" "*") ("call c2 to add hello with" 0) ("call c3 to add bye with" 0)))
(test (c2 0) '(("Current value:" "*hello") ("call c2 to add hello with" 1) ("call c3 to add bye with" 1)))
(test (c2 0) '(("Current value:" "*hello") ("call c2 to add hello with" 2) ("call c3 to add bye with" 2)))
(test (c3 2) '(("Current value:" "*hellobye") ("call c2 to add hello with" 3) ("call c3 to add bye with" 3)))
