#lang racket

(define (magic-rows M)
  (cond [(apply = (map (lambda (x) (apply + x)) M)) (apply + (first M))]
        [else #f]))

(define (transpose M)
  (cond [(null? (first (map rest M))) (list (map first M))]
        [else (cons (map first M) (transpose (map rest M)))]))

(define (magic-cols M)
  (magic-rows (transpose M)))

(define (first-diagonal-sum M)
  (cond [(null? M) 0]
  [else (+ (first (first M)) (first-diagonal-sum (rest (map rest M))))]))

(define (second-diagonal-sum M)
  (first-diagonal-sum (reverse M)))

(define (magic-square? M)
  (cond [(not (or (magic-rows M) (magic-cols M))) #f]
        [else (apply = (list (magic-rows M) (magic-cols M) (first-diagonal-sum M) (second-diagonal-sum M)))]))
  