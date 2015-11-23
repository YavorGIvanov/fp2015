#lang racket

;iterative
(define (reverse2 items)
  (define (reverse2-iter newitems result)
    (cond [(null? newitems) result]
          [else (reverse2-iter (rest newitems) (cons (first newitems) result))]))
  (reverse2-iter items '()))

;standart recursive
(define (reverse3 items)
  (cond [(null? items) '()]
        [else (append (reverse3 (rest items)) (list (first items)))]))