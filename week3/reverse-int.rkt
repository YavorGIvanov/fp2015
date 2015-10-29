#lang racket

(provide palindrome? reverse-int)

(define (reverse-int n)
  (define (reverse-iter newn result)
    (cond [(= newn 0) result]
          [else (reverse-iter (quotient newn 10) (+ (remainder newn 10) (* result 10)))]))
  (reverse-iter n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

