#lang sicp

(#%require rackunit)

(define (accumulate combiner null_value term a next b)
  (if (> a b)
    null_value
    (combiner (term a)
	      (accumulate combiner null_value term (next a) next b))))

(define (accumulate_iter combiner null_value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null_value))

(check-equal? (accumulate + 0 identity 1 inc 5) 15)
(check-equal? (accumulate * 1 identity 1 inc 5) 120)

(check-equal? (accumulate_iter + 0 identity 1 inc 5) 15)
(check-equal? (accumulate_iter * 1 identity 1 inc 5) 120)
