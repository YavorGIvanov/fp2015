#lang racket

(require "../week5/take-while.rkt")
(require "../week5/drop-while.rkt")

;Encode
;It is possible to be done in a simpler manner (I guess) :)
(define (run-length-encode str)
  (letrec ([repeats-first (lambda (items)
                            (let ([len (length (take-while (lambda (x) (equal? x (first items))) items))])
                              (if (= len 1) "" len)))]
           [encode (lambda (items)
                     (cond [(null? items) '()]
                           [else (cons (list (repeats-first items) (first items))
                                       (encode (drop-while (lambda (x) (equal? x (first items))) items)))]))])
    (apply ~a (apply append (encode (string->list str))))))

;Decode
(define (run-length-decode str)
  (letrec ([string-repeat (lambda (str times)
                            (cond [(= times 0) ""]
                                  [else (string-append str (string-repeat str (- times 1)))]))]
           [decode (lambda (items)
                     (let ([withoutNums (drop-while char-numeric? items)])
                       (cond [(null? items) ""]
                             [else (string-append (string-repeat
                                                   (apply ~a (take-while char-alphabetic? withoutNums))
                                                   (string->number (apply ~a (take-while char-numeric? items))))
                                                  (decode (drop-while char-alphabetic? withoutNums)))])))])
    (decode (string->list str))))




