#lang racket

(define (where items ps)
  (cond [(null? ps) items]
        [(where (filter (first ps) items) (rest ps))]))