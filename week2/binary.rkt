#lang racket
(provide
 string-reverse
 to-binary-string)
 
(define (string-reverse str)
  (define (str-rev-iter rev pos)
    (cond [(< pos 0) rev]
          [else (str-rev-iter (string-append rev (~a (string-ref str pos))) (- pos 1))]))
  (str-rev-iter "" (- (string-length str) 1)))

(define (to-binary-string n)
  (define (to-bin-str-iter tempn result)
    (cond [(= tempn 1) (string-append result "1")]
          [else (to-bin-str-iter (quotient tempn 2) (string-append result (~a (remainder tempn 2))))]))
  (if (<= n 0) (~a 0)
               (string-reverse (to-bin-str-iter n ""))))

(define (from-binary-string str)
  (define (from-bin-str-iter result pos)
    (cond [(= pos (string-length str)) result]
          [else (from-bin-str-iter (+ result (* (string->number (~a (string-ref str (- (- (string-length str) 1) pos)))) (expt 2 pos))) (+ pos 1))]))
  (if (< (string->number str) 0) 0 (from-bin-str-iter 0 0)))
