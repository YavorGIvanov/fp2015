#lang racket

(require "range.rkt")

(define (build-list2 f n)
 (cond [(<= n 0) '()]
       [else (append (build-list2 f (- n 1)) (list (f (- n 1))))]))

(define (build-list3 f n)
 (define (build-list-iter curr result)
       (cond [(= curr n) result]
             [else (build-list-iter (+ curr 1) (append result (f curr)))]))
  (build-list-iter 0 '()))

(define (build-list4 f n)
  (map f (range 0 n)))