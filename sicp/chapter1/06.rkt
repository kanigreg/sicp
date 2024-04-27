#lang sicp

(#%require rackunit)

(define (new-if predicate then-clause else-clause)
  (cond [predicate then-clause]
        [else else-clause]))


(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                      x)))

(define (improve guess x)
  (avarage guess (/ x guess)))

(define (avarage x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

; (define (sqrt x) (sqrt-iter 1.0 x))

; (sqrt 9)

; Программа не выполнится. Предположительно из-за того, что в новой версии new-if вычисляются оба операнда.
