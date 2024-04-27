#lang sicp

(#%require rackunit)

(define (square x)
  (* x x))

(define (sum_of_square a b)
  (+ (square a) (square b)))

(define (solution a b c)
  (cond [(and (< c a) (< c b)) (sum_of_square a b)]
        [(and (< b a) (< b c)) (sum_of_square a c)]
        [(and (< a b) (< a c)) (sum_of_square b c)]
        [else ("impossible")]))

(check-equal? (solution 1 2 3) 13)
(check-equal? (solution 11 2 3) 130)
(check-equal? (solution 4 3 2) 25)

