#lang racket

(define (prime? n)
   (define (prime-iter? counter)
     (cond [(> counter (sqrt n)) #t]
           [(= (remainder n counter) 0) #f]
           [else (prime-iter? (+ counter 1))]))
  (if (< n 2) #f (prime-iter? 2)))