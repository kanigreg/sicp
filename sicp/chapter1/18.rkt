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

(check-equal? (iter_mult 1 1) 1)
(check-equal? (iter_mult 40 30) (* 40 30))
(check-equal? (iter_mult 5 0) 0)
(check-equal? (iter_mult 5 15) (* 5 15))
