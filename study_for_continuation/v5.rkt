#lang plai

(define total empty)

(define (remember v)
  (local [(define key (length total))]
    (begin (set! total (append total (list v)))
           key)))

(define (lookup index)
  (list-ref total index))

(define (f)
  (do-f 0))

(define (resume/k key val)
  (local [(define cont (lookup key))]
    (cont val)))

(define (do-f total)
  (web-read/k (format "Total is ~a\n" total)
              (lambda (val)
                (do-f (+ val total)))))

(define (web-read/k total-for cont)
  (local [(define key (remember cont))]
    `(,total-for "call resume/k with " ,key "and value")))
