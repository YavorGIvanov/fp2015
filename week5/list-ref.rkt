#lang racket

(define (list-ref2 items n)
  (cond [(<= n 0) (first items)]
        [else (list-ref2 (rest items) (- n 1))]))