#lang sicp

(#%require rackunit)

(define (pascal_triangle tier position)
  (if (or (< tier 2) (< position 2) (= position tier))
      1
      (+ (pascal_triangle (- tier 1)
                          position)
         (pascal_triangle (- tier 1)
                          (- position 1)))))

(check-equal? (pascal_triangle 1 1) 1)
(check-equal? (pascal_triangle 3 2) 2)
(check-equal? (pascal_triangle 4 3) 3)
(check-equal? (pascal_triangle 5 2) 4)
(check-equal? (pascal_triangle 5 3) 6)
