#lang racket

(provide take3)

;recursive
(define (take2 n items)
  (cond [(> n (length items)) '()]
        [(= n 0) '()]
        [else (cons (first items) (take2 (- n 1) (rest items)))]))

;iterative
(define (take3 n items)
  (define (take3-iter curr newitems result)
    (cond [(= n curr) result]
          [else (take3-iter (+ curr 1) (rest newitems) (append result (list (first newitems))))]))
  (if (> n (length items)) '() (take3-iter 0 items '())))