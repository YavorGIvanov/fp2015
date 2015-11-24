#lang racket
(provide make-tree make-leaf empty-tree? root left right count-nodes height t leaf?)

(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (leaf? tree)
  (and (empty? (left tree)) (empty? (right tree))))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (count-nodes tree)
  (cond [(empty-tree? tree) 0]
        [else (+ 1 (count-nodes (left tree)) (count-nodes (right tree)))]))

(define (height tree)
 (cond  [(empty-tree? tree) 0]
        [else (+ 1 (max (height (left tree)) (height (right tree))))]))

(define t (make-tree 5
             (make-tree 3
                        (make-leaf 2)
                        (make-leaf 4))
             (make-tree 10
                        (make-leaf 7)
                        (make-tree 20
                                   (make-leaf 15)
                                   (make-leaf 25)))))