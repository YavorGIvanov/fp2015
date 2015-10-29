#lang racket
(define (product-digits n)
  (if (< n 10) n (* (remainder n 10) (product-digits (quotient n 10)))))