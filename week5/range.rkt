#lang racket

(provide range2)

(define (range2 a b)
  (cond [(>= a b) '()]
        [else (cons a (range2 (+ a 1) b))]))