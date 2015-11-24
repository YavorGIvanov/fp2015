#lang racket

(require "../week5/take-while.rkt")
(require "../week5/drop-while.rkt")
(provide group)

(define (group items)
  (cond [(null? items) '()]
        [else (cons
               (take-while (lambda (x) (equal? x (first items))) items)
               (group (drop-while (lambda (x) (equal? x (first items))) items)))]))