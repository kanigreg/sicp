#lang sicp

(#%require rackunit)

(define (add_rat x y)
  (make_rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul_rat x y)
  (make_rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (make_rat n d)
  (let ((g (gcd n d))
	(sign (if (< d 0) -1 1)))
    (cons (/ n g sign) (/ d g sign))))

(define (equal_rat? x y)
  (and (= (numer x) (numer y)) 
       (= (denom x) (denom y))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print_rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

#| TESTS |#

(check-true (equal_rat? (make_rat -1 -3)
			(make_rat 1 3)))
(check-true (equal_rat? (make_rat 1 -3)
			(make_rat -1 3)))
(check-true (equal_rat? (add_rat (make_rat 1 -3)
				 (make_rat -1 2))
			(make_rat -5 6)))
(check-true (equal_rat? (mul_rat (make_rat 1 -3)
				 (make_rat -1 2))
			(make_rat 1 6)))
