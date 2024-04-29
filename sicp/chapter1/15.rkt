#lang sicp

(#%require rackunit)

; предполагаем что для достаточно малого угла `sin x = x`

(define (cube x) (* x x x))

(define (p x) (- x (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

; a. (sine 12.15) вызывается 5 раз
;
; b. по мере увеличения угла, порядок фукнции возрастает
; логарифмически по основанию 3
