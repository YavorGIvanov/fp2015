#lang racket

(define (append2 l1 l2)
  (cond [(null? l1) l2]
        [else (cons (first l1) (append2 (rest l1) l2))]))