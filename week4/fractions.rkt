#lang racket
;There are some additional stuff I put in here
(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (frac-operations str)
  (cond [(string=? str "+") add-frac]
        [(string=? str "-") substract-frac]
        [(string=? str "*") mult-frac]
        [(string=? str "/") divide-frac]
        [else add-frac]))

(define (gcd n1 n2)
  (cond [(= n2 0) n1]
        [(= n2 1) 1]
        [else (gcd n2 (remainder n1 n2))]))

(define (lcm n1 n2)
  (/ (* n1 n2) (gcd n1 n2)))

(define (add-frac frac1 frac2)
  (add-and-substract-frac + frac1 frac2))

(define (substract-frac frac1 frac2)
  (add-and-substract-frac - frac1 frac2))

(define (add-and-substract-frac operation frac1 frac2)
      (let ([lcmValue (lcm (snd frac1) (snd frac2))])
        (simplify-frac (cons (operation
                              (* (/ lcmValue (snd frac1)) (fst frac1))
                              (* (/ lcmValue (snd frac2)) (fst frac2)))
                             lcmValue))))

(define (mult-frac frac1 frac2)
  (simplify-frac
   (cons (* (fst frac1) (fst frac2))
         (* (snd frac1) (snd frac2)))))

(define (divide-frac frac1 frac2)
  (mult-frac frac1 (cons (snd frac2) (fst frac2))))

(define (simplify-frac frac)
  (let ([first (fst frac)]
        [second (snd frac)])
    (if (or (= first 0) (= second 0)) 0
        (cons (/ first (gcd first second)) (/ second (gcd first second))))))