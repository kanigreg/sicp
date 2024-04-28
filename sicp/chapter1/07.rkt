#lang sicp

(#%require rackunit)

(define (sqrt_iter current_guess prev_guess x)
  (if (good_enough? current_guess prev_guess)
          current_guess
          (sqrt_iter (improve current_guess x)
                     current_guess
                     x)))

(define (improve guess x)
  (avarage guess (/ x guess)))

(define (avarage x y)
  (/ (+ x y) 2))

(define epsilon 0.000001)

(define (good_enough? current_guess prev_guess)
  (< (abs (- current_guess prev_guess)) epsilon))

(define (sqrt x) (sqrt_iter
                   (improve 1.0 x)
                   1.0
                   x))

(check-equal? (sqrt 9) 3.0)
