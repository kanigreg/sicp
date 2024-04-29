#lang sicp

(#%require rackunit)

(define (iter_expt number power)
  (define (iter num iter_power acc)
    (cond [(= iter_power 0) acc]
          [(odd? iter_power) (iter num
                                  (- iter_power 1)
                                  (* num acc))]
          [else (iter (* num num)
                      (/ iter_power 2)
                      acc)]))
  (if (= power 0)
      1
      (iter number
            power
            1)))

(check-equal? (iter_expt 2 0) 1)
(check-equal? (iter_expt 2 2) 4)
(check-equal? (iter_expt 2 8) 256)
(check-equal? (iter_expt 3 3) 27)
(check-equal? (iter_expt 3 6) 729)
