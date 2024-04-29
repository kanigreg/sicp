#lang sicp

(#%require rackunit)

(define (square number) (* number number))

(define (fib n)
  (fib_iter 1 0 0 1 n))

(define (fib_iter a b p q counter)
  (cond [(= counter 0) b]
        [(even? counter) (fib_iter a
                                  b
                                  (+ (square q) (square p))
                                  (+ (square q) (* 2 p q))
                                  (/ counter 2))]
        [else (fib_iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- counter 1))]))

(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)
(check-equal? (fib 5) 5)
(check-equal? (fib 7) 13)
(check-equal? (fib 8) 21)
(check-equal? (fib 10) 55)
