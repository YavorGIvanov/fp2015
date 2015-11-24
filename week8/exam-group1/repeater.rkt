#lang racket

(define (repeater str)
  (letrec ([repeat (lambda (count glue)
                     (cond [(= count 0) ""]
                           [else (string-append str glue (repeat (- count 1) glue))]))
                   ])
    repeat))