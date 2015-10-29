#lang racket
(require "../week2/binary.rkt")

(define (count-binary-ones n)
  (define (count-binary-ones-iter count str pos)
  (cond [(< pos 0) count]
        [(string=? (~a (string-ref str pos)) "1") (count-binary-ones-iter (+ count 1) str (- pos 1))]
        [else (count-binary-ones-iter count str (- pos 1))]))
  (count-binary-ones-iter 0 (to-binary-string n) (- (string-length (to-binary-string n)) 1)))
         
(define (is-hack-num? n)
  (cond [(< n 1) #f]
        [(and (string=? (string-reverse (to-binary-string n)) (to-binary-string n)) (odd? (count-binary-ones n))) #t]
        [else #f]))
  
(define (next-hack n)
  (if (is-hack-num? (+ n 1)) (+ n 1) (next-hack (+ n 1))))