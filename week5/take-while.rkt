#lang racket

(provide take-while)

(define (take-while p items)
  (cond [(null? items) '()]
        [(not (p (first items))) '()]
        [else (cons (first items) (take-while p (rest items)))]))