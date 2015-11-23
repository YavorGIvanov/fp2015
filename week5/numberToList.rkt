#lang racket

;recursive
;(number->list 0) -> '()
;(number->list -1234) -> (-1 -2 -3 -4)
;the iterative version is better(it is possible to better this version too)
(define (number->list n)
  (cond [(= n 0) '()]
        [else (append
               (number->list (quotient n 10))
               (cons (remainder n 10) '()))]))

;iterative
(define (number->list2 n)
  (define (iter newn result)
    (cond [(= newn 0) result]
          [else (iter (quotient newn 10) (cons (remainder newn 10) result))]))
  (cond [(zero? n) (cons 0 '())]
        [(< n 0) (addMinusTo1st (iter (* -1 n) '()))] ; that's such a bullshit
        [else (iter n '())]))

(define (addMinusTo1st listofNum)
  (append (cons (* -1 (first listofNum)) '()) (rest listofNum)))