#lang sicp

(#%require rackunit)

(define (square n)
  (* n n))

(define (fast_expt b n)
  (cond [(= n 0) 1]
	[(even? n) (square (fast_expt b (/ n 2)))]
	[else (* b (fast_expt b (- n 1)))]))

(define (expmod base exp m)
  (remainder (fast_expt base exp) m))

(define (fermat_teset n)
  (define (try_it a)
    (= (expmod a n n) a))
  (try_it (+ 1 (random (- n 1)))))

(define (fast_prime? n times)
  (cond [(= times 0) true]
	[(fermat_teset n) (fast_prime? n (- times 1))]
	[else false]))

(define (timed_prime_test n)
  (start_prime_test n (runtime)))

(define (start_prime_test n start_time)
  (if (fast_prime? n 10)
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
