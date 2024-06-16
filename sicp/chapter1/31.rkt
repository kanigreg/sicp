#lang sicp

(#%require rackunit)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (product_iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (if (= n 0)
      1
      (product_iter identity 1 inc n)))

(check-equal? (product inc 1 inc 3) 24)
(check-equal? (product_iter inc 0 inc 3) 24)
(check-equal? (factorial 5) 120)
