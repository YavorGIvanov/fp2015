#lang racket
(provide string-repeat)

(define (string-repeat str num)
 (define (str-rep-iter result newn)
   (cond [(= newn 1) result]
   [else (str-rep-iter (string-append result str) (- newn 1))]))
  (if (< num 1) "" (str-rep-iter str num)))
  
(define (fences n)
  (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">"
                 (~a n)
                 "<" (string-repeat "-" (round (+ 1 (log n)))) "}"))