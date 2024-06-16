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

(define (fermat-teset n)
  (define (try_it a)
    (= (expmod a n n) a))
  (try_it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
	[(fermat-teset n) (fast-prime? n (- times 1))]
	[else false]))

(define (timed_prime_test n)
  (start_prime_test n (runtime)))

(define (start_prime_test n start_time)
  (if (fast-prime? n 10)
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

#| (search_for_primes 1000 0) |#
#| (search_for_primes 10000 0) |#
#| (search_for_primes 100000 0) |#
#| (search_for_primes 1000000 0) |#
#| (search_for_primes 10000000 0) |#
#| (search_for_primes 100000000 0) |#
#| (search_for_primes 1000000000 0) |#
