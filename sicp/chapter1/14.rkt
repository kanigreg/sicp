#lang sicp

(#%require rackunit)

(define (count_change amount)
  (cc amount 5))

(define (cc amount kind_of_coins)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (= kind_of_coins 0)) 0]
        [else (+ (cc amount
                     (- kind_of_coins 1))
                 (cc (- amount
                        (first_denomination kind_of_coins))
                 kind_of_coins))]))

(define (first_denomination kind_of_coins)
  (cond [(= kind_of_coins 1) 1]
        [(= kind_of_coins 2) 5]
        [(= kind_of_coins 3) 10]
        [(= kind_of_coins 4) 25]
        [(= kind_of_coins 5) 50]
        [else "unhendled error"]))

(check-equal? (count_change 100) 292)
