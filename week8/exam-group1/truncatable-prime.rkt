#lang racket

(define (prime? x)
  (define (prime-helper? i)
    (cond [(> i (sqrt x)) #t]
          [(= (remainder x i) 0) #f]
          [else (prime-helper? (+ i 1))]))
 (if (<= x 1) #f (prime-helper? 2)))

(define (truncatable-prime? x)
  (cond [(< x 10) (prime? x)]
        [(prime? x) (truncatable-prime? (quotient x 10))]
        [else #f]))