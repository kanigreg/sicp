#lang sicp

(#%require rackunit)

(define (square n) (* n n))

(define (smallest_divisor n)
  (find_divisor n 2))

(define (find_divisor n test_divisor)
  (cond [(> (* test_divisor test_divisor) n) n]
        [(divides? test_divisor n) test_divisor]
        [else (find_divisor n (+ test_divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (= n 1)
      false
      (= (smallest_divisor n) n)))

(define (filtered_accumulate_iter combiner null_value predicat term a next b)
  (define (filtered_value x)
    (if (or (predicat x) (> x b))
      x
      (filtered_value (next x))))
  (define (iter a result)
    (if (> a b)
	result
	(iter (filtered_value (next a)) (combiner (term a) result))))
  (iter (filtered_value a) null_value))

(define (square_prime_sum a b)
  (filtered_accumulate_iter + 0 prime? square a inc b))

(check-equal? (square_prime_sum 1 10) 87) ; 2*2 + 3*3 + 5*5 + 7*7
