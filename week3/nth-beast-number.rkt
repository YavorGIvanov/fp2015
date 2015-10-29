#lang racket
(require "../week2/fence.rkt")

(define (nth-beast-number n)
  (if (< n 1) 0 (string->number (string-repeat "666" n))))
