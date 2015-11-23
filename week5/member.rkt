#lang racket

(define (member? x items)
  (cond [(null? items) #f]
        [(equal? x (first items)) #t]
        [else (member? x (rest items))]))