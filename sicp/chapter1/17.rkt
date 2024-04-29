#lang sicp

(#%require rackunit)

(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (iter_mult a b)
  (define (iter acc left right)
    (cond [(= right 0) acc]
          [(odd? right) (iter (+ left acc)
                             left
                             (- right 1))]
          [else (iter acc
                      (double left)
                      (halve right))]))
  (if (= a 0) 0 (iter 0 a b)))

(check-equal? (iter_mult 2 0) 0)
(check-equal? (iter_mult 0 2) 0)
(check-equal? (iter_mult 2 2) 4)
(check-equal? (iter_mult 2 1) 2)
(check-equal? (iter_mult 1 2) 2)
(check-equal? (iter_mult 11 11) 121)

