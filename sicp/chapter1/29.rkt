#lang sicp

(#%require rackunit)

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2)) add-dx b)))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (add_2h x) (+ x (* 2 h)))
  (* (/ h 3.0)
     (+ (f a)
	(f b)
	(* 2 (sum f (add_2h a) add_2h (- b h)))
	(* 4 (sum f (+ a h) add_2h b)))))

(check-equal? (integral-simpson cube 0 1 100) 0.25)
