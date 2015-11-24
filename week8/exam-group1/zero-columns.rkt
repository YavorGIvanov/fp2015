#lang racket

(define (zero-columns matrix)
  (cond [(null? (first matrix)) matrix]
        [(containzero? (map first matrix)) (map cons (map (lambda (x) 0) (map first matrix)) (zero-columns (map rest matrix)))]
        [else (map cons (map first matrix) (zero-columns (map rest matrix)))]))

(define (containzero? l)
  (< 0 (apply + (map (lambda (x) (if (zero? x) 1 0)) l))))
 