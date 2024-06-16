#lang sicp

(#%require rackunit)

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond [(= exp 0) 1]
	[(even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m)]
	[else
	  (remainder (* base (expmod base (- exp 1) m))
		     m)]))

(define (fermat-test n)
  (define (try_it a)
    (= (expmod a n n) a))
  (try_it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
	[(fermat-test n) (fast-prime? n (- times 1))]
	[else false]))

(define (smallest_divisor n)
  (find_divisor n 2))

(define (find_divisor n test_divisor)
  (cond [(> (square test_divisor) n) n]
	[(divides? test_divisor n) test_divisor]
	[else (find_divisor n (+ test_divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest_divisor n)))

(check-false (prime? 561))
(check-true (fast-prime? 561 10)) ; false positive
(check-false (prime? 1105))
(check-true (fast-prime? 1105 10)); false positive
(check-false (prime? 6601))
(check-true (fast-prime? 6601 10)) ; false positive
