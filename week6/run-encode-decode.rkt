#lang racket

(require "../week5/take-while.rkt")
(require "../week5/drop-while.rkt")
(require "group.rkt")

;Encode
(define (run-length-encode str) ;beta version(not using group)
  (letrec ([repeats-first (lambda (items)
                            (let ([len (length (take-while (lambda (x) (equal? x (first items))) items))])
                              (if (= len 1) "" len)))]
           [encode (lambda (items)
                     (cond [(null? items) '()]
                           [else (cons (list (repeats-first items) (first items))
                                       (encode (drop-while (lambda (x) (equal? x (first items))) items)))]))])
    (apply ~a (apply append (encode (string->list str))))))

;Decent Version of Encode
(define (run-length-encode2 str) ;version 0.1(using group)
  (define (iter l result)
    (if (null? l) result
        (let ([len (length (first l))]
              [firstLetter (~a (first (first l)))])
          (cond [(= len 1) (iter (rest l) (string-append result firstLetter))]
                [else (iter (rest l) (string-append result (string-append (~a len) firstLetter)))]))))
  (iter (group (string->list str)) ""))

;Decode
(define (run-length-decode str) ;beta version
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




