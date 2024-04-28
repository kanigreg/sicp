#lang sicp

(#%require rackunit)

(define (f n)
  (if (< n 3)
        n
        (+ (f (- n 1))
           (f (- n 2))
           (f (- n 3)))))

(check-equal? (f 1) 1)
(check-equal? (f 3) 3)
(check-equal? (f 4) 6)
(check-equal? (f 6) 20)

(define (g n)
  (define (f_iter first_prev second_prev third_prev current_count)
    (if (= current_count n)
        first_prev
        (f_iter (+ first_prev second_prev third_prev)
           first_prev
           second_prev
           (+ current_count 1))))
  (if (< n 4)
      n
      (f_iter 3 2 1 3)))

(check-equal? (g 1) 1)
(check-equal? (g 3) 3)
(check-equal? (g 4) 6)
(check-equal? (g 6) 20)
