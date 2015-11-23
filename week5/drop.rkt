#lang racket

(define (drop2 n items)
  (cond [(> n (length items)) '()]
        [(= n 0) items]
        [else (drop2 (- n 1) (rest items))]))