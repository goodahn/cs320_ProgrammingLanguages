#lang plai


(define (d)
  (do-d 0))

(define (do-d val)
  (begin (printf "Total is ~a\n
                 Add 2 next?\n"
                 val)
         (do-d (+ val (if (read) 2 3)))))

