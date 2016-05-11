#lang plai

(define total empty)

(define (lookup index)
  (list-ref total index))

(define (remember val)
  (local [(define n (length total))]
    (begin (set! total (append total (list val)))
           n)))

(define (g)
  (do-g 0))

(define (do-g val)
  (web-read/k "call resume/k with " val 
              (lambda (val1)
                (web-read/k "call resume/k with " (+ val val1) 
                            (lambda (val2)
                              (web-read/k "call p-resume/k with " (+ val val1 val2) 
                                          (lambda ()
                                            (do-g (+ val val1 val2)))))))))

(define (web-read/k explain val cont)
  (local [(define key (remember cont))]
    (if (string=? explain "call resume/k with ")
      `(("current value: " ,val) ,explain ,key "and value")
      `(("current value: " ,val) ,explain ,key))))

(define (resume/k key val)
  ((lookup key) val))

(define (p-resume/k key)
  ((lookup key)))
