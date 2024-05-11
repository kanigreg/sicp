#lang sicp

(#%require rackunit)

(define (smallest_divisor n)
  (find_divisor n 2))

(define (find_divisor n test_divisor)
  (cond [(> (* test_divisor test_divisor) n) n]
        [(divides? test_divisor n) test_divisor]
        [else (find_divisor n (+ test_divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest_divisor n) n))

(check-true (prime? 199))
(check-true (prime? 1999))
(check-false (prime? 19999))
