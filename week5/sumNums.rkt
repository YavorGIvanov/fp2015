#lang racket

;(apply + number)
(define (sum numbers)
  (define (sum-iter nums result)
    (cond [(null? nums) result]
          [else (sum-iter (rest nums) (+ result (first nums)))]))
  (sum-iter numbers 0))