#lang plai
;; natural?: number -> boolean
;; check whether number is non negative integer
(define (natural? num)
  (and (integer? num) (> num -1)))

;; intelligence? : symbol -> boolean
;; check whether symbol is in { 'dumb ,'average , 'smart }
(define (intelligence? intelli)
  (and (symbol? intelli) 
       (or (eqv? intelli 'dumb) (eqv? intelli 'average) (eqv? intelli 'smart))))
  
(define-type ANIMAL
  [spider (legs natural?)
          (space natural?)]
  [elephant (space natural?)]
  [monkey (intelligence intelligence?)
          (space natural?)])

(define m-spider (spider 5 4))
(define m-elephant (elephant 4))
(define d-monkey (monkey 'dumb 4))
(define s-monkey (monkey 'smart 4))
(define a-monkey (monkey 'average 0))
(test (spider-legs m-spider) 5)
(test (spider-space m-spider) 4)
(test (elephant-space m-elephant) 4)
(test (monkey-intelligence d-monkey) 'dumb)
(test (monkey-intelligence s-monkey) 'smart)
(test (monkey-intelligence a-monkey) 'average)
(test (monkey-space a-monkey) 0)

;; need-space: ANIMAL -> non negative integer
;; get space units needed to transport animal
(define (need-space animal)
  (if (ANIMAL? animal)
      (cond
        [(spider? animal) (spider-space animal)]
        [(elephant? animal) (elephant-space animal)]
        [(monkey? animal) (monkey-space animal)])
      (error "Only animal please")))

(test (need-space m-spider) 4)
(test (need-space m-elephant) 4)
(test (need-space d-monkey) 4)
(test/exn (need-space 5) "Only animal please")
(test/exn (need-space 'dumb) "Only animal please")

;; can-talk: ANIMAL -> boolean
;; check whether animal is smart monkey
(define (can-talk animal)
  (if (ANIMAL? animal)
      (if (monkey? animal)
          (eqv? (monkey-intelligence animal) 'smart)
          #f)
      (error "Only animal please")))

(test (can-talk s-monkey) #t)
(test (can-talk d-monkey) #f)
(test (can-talk a-monkey) #f)
(test (can-talk m-spider) #f)
(test (can-talk m-elephant) #f)
(test/exn (can-talk 'a) "Only animal please")

;; name-toys: list_of_symbol -> list_of_symbol
;; replace 'robot with 'bb8, 'doll with 'baymax, 'bear with 'pooh in list, and keeps the other toys as unnamed
(define (name-toys toys)
  (if (list? toys)
      (if (empty? toys)
          toys
          (if (list? (first toys))
              (cons (name-toys (first toys)) (name-toys (rest toys)))
              (cond 
                [(not (symbol? (first toys))) (error "Only symbol please")]
                [(eqv? (first toys) 'robot) (cons 'bb8 (name-toys (rest toys)))]
                [(eqv? (first toys) 'doll) (cons 'baymax (name-toys (rest toys)))]
                [(eqv? (first toys) 'bear) (cons 'pooh (name-toys (rest toys)))]
                [else (cons (first toys) (name-toys (rest toys)))])))
      (error "Only list please")))
      
(test (name-toys (list 'robot 'doll 'bear 'bb8)) (list 'bb8 'baymax 'pooh 'bb8))
(test (name-toys (list 'a 'doll 'bear 'bb8)) (list 'a 'baymax 'pooh 'bb8))
(test (name-toys (list (list 'robot 'doll) 'bear 'bb8)) 
      (list (list 'bb8 'baymax) 'pooh 'bb8))
(test (name-toys (list 'a 'b 'c 'd)) (list 'a 'b 'c 'd))
(test (name-toys (list (list 'robot 'doll) (list 'bear 'bb8))) 
      (list (list 'bb8 'baymax) (list 'pooh 'bb8)))
(test (name-toys (list (list (list 'robot 'doll) (list 'bear 'bb8)))) 
      (list (list (list 'bb8 'baymax) (list 'pooh 'bb8))))
(test/exn (name-toys (list (list 'robot 'doll) (list 'bear 'bb8) 5)) 
      "Only symbol please")
(test/exn (name-toys 5) "Only list please")

;; give-name: symbol symbol list_of_symbol -> list_of_symbol
;; change old symbol to new symbol in given list
(define (give-name old new toys)
  (if (and (symbol? old) (symbol? new))
      (if (list? toys)
          (if (empty? toys)
              toys
              (if (list? (first toys))
                  (cons (give-name old new (first toys)) (give-name old new (rest toys)))
                  (cond 
                    [(not (symbol? (first toys))) (error "Only symbol please")]
                    [(eqv? (first toys) old) (cons new (give-name old new (rest toys)))]
                    [else (cons (first toys) (give-name old new (rest toys)))])))
          (error "Only list please"))
      (error "You should give me old name and new name by symbol")))

(test (give-name 'robot 'hubo (list 'doll 'robot 'bear)) (list 'doll 'hubo 'bear))
(test (give-name 'robot 'hubo (list 'a 'b 'c 'robot)) (list 'a 'b 'c 'hubo))
(test (give-name 'robot 'hubo (list 'a (list 'b 'c) 'robot)) (list 'a (list 'b 'c) 'hubo))
(test (give-name 'robot 'hubo (list 'a (list 'robot 'c) 'robot)) (list 'a (list 'hubo 'c) 'hubo))
(test (give-name 'robot 'hubo (list 'a 'b 'c 'd)) (list 'a 'b 'c 'd))
(test/exn (give-name 'robot 'hubo (list 'a (list 'robot 'c) 5)) "Only symbol please")
(test/exn (give-name 'robot 'hubo 5) "Only list please")
(test/exn (give-name 5 'hubo (list 'a 'b 5)) "You should give me old name and new name by symbol")