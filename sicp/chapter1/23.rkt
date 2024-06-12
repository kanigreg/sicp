
#lang sicp

(#%require rackunit)

(define (smallest_divisor n)
  (find_divisor n 2))

(define (square n)
  (* n n))

(define (find_divisor n test_divisor)
  (cond [(> (square test_divisor) n) n]
	[(divides? test_divisor n) test_divisor]
	[else (find_divisor n (iter_test_divisor test_divisor))]))

(define (iter_test_divisor current)
  #| (+ current 1)) |#
  (if (= current 2)
    3
    (+ current 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest_divisor n)))

(define (timed_prime_test n)
  (start_prime_test n (runtime)))

(define (start_prime_test n start_time)
  (if (prime? n)
    (report_prime (- (runtime) start_time) n)
    #f))

(define (report_prime elapsed_time n)
  (display n)
  (display " *** ")
  (display elapsed_time)
  (newline))

(define (search_for_primes from count)
  (if (< count 3)
    (if (timed_prime_test from)
      (search_for_primes (+ from 1) (+ count 1))
      (search_for_primes (+ from 1) count))
    (display "Done\n")))

(search_for_primes 1000 0)
(search_for_primes 10000 0)
(search_for_primes 100000 0)
(search_for_primes 1000000 0)
(search_for_primes 10000000 0)
(search_for_primes 100000000 0)
(search_for_primes 1000000000 0)
