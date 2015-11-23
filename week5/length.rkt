#lang racket

(define (length2 items)
  (define (length-iter newitems len)
    (cond [(null? newitems) len]
          [else (length-iter (cdr newitems) (+ len 1))]))
  (length-iter items 0))