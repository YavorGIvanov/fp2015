#lang racket

(provide drop-while)

(define (drop-while p items)
  (cond [(null? items) '()]
        [(not (p (first items))) items]
        [else (drop-while p (rest items))]))

