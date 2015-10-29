#lang racket
(define (cube-sums? n)
  (define (cube-sums-iter? a b)
    (cond
      [(> b (round(expt n (/ 1 3)))) #f]
      [(= a (expt (round(expt a (/ 1 3))) 3)) #t]
      [else (cube-sums-iter? (- n (expt b 3)) (+ b 1))]))
  (cond [(= n 2) #t]
        [(< n 2) #f]
        [else (cube-sums-iter? (- n 1) 1)]))