#lang sicp

(#%require rackunit)

(define (cube_root x)
  (cube_root_iter (improve 1.0 x)
                  1.0
                  x))

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

(define (cube_root_iter current_guess prev_guess x)
  (if (good_enoght? current_guess prev_guess)
    current_guess
    (cube_root_iter (improve current_guess x)
                    current_guess
                    x)))

(define epsilon 0.0000001)

(define (good_enoght? current_guess prev_guess)
  (< (abs (- current_guess prev_guess)) epsilon))

(check-equal? (cube_root 8) 2.0)
(check-equal? (cube_root 27) 3.0)
(check-equal? (cube_root 1331) 11.0)
