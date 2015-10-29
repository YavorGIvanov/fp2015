#lang racket
(define (series a b n)
  (cond [(= n 1) a]
        [(= n 2) b]
        [else (series b (+ a b) (- n 1))]))
(define (lucas n)
  (series 2 1 n))

(define (fibonacci n)
  (series 1 1 n))

(define (summed-member n)
  (+ (lucas n) (fibonacci n)))

(define (nth-lucas-sum n)
  (define (nth-lucas-sum-iter curr sum)
    (if (= curr n) (+ sum (lucas curr)) (nth-lucas-sum-iter (+ curr 1) (+ sum (lucas curr)))))
  (nth-lucas-sum-iter 1 0))

(define (nth-fibonacci-sum n)
  (define (nth-fibonacci-sum-iter curr sum)
    (if (= curr n) (+ sum (fibonacci curr)) (nth-fibonacci-sum-iter (+ curr 1) (+ sum (fibonacci curr)))))
  (nth-fibonacci-sum-iter 1 0))

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))
