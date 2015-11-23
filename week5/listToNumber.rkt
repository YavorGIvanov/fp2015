#lang racket

;recursive
(define (list->number ns)
  (cond [(null? ns) 0]
        [else (+ (* (first ns) (expt 10 (- (length ns) 1))) (list->number (rest ns)))]))

;iterative
(define (list->number2 ns)
  (define (iter newns result)
    (cond [(null? newns) result]
          [else (iter (rest newns) (+ (* 10 result) (first newns)))]))
  (iter ns 0))