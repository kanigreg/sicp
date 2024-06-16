#lang sicp

(define (expmod base exp m)
  (cond [(= exp 0) 1]
	[(even? exp)
	 (remainder (* (expmod base (/ exp 2) m)
		       (expmod base (/ exp 2) m))
		    m)]
	[else
	  (remainder (* base (expmod base (- exp 1) m))
		     m)]))

; Здесь процесс порядка O(n), т.к. при вычислении четной экспоненты происходит 2 раза
; на каждом шаге итерации. Это приводит к кратному увеличению порядка.
; Если раньше процесс выполнялся за логарифм в основании 2 то теперь за log(n) * log(n) = n.
