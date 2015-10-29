#lang racket
(require "reverse-int.rkt")

(define (p-score n)
  (define (p-score-iter newn count)
    (if (palindrome? newn) count (p-score-iter (+ newn (reverse-int newn)) (+ count 1))))
  (p-score-iter n 1))