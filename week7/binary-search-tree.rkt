#lang racket

(require "tree.rkt")

(define (bst-insert x tree)
  (cond [(empty-tree? tree) (make-leaf x)]
        [(< x (root tree)) (make-tree (root tree) (bst-insert x (left tree)) (right tree))]
        [else (make-tree (root tree) (left tree) (bst-insert x (right tree)))]))

(define (bst-element? x tree)
  (cond [(empty-tree? tree) #f]
        [(equal? x (root tree)) #t]
        [(< x (root tree)) (bst-element? x (left tree))]
        [else (bst-element? x (right tree))]))

(define (bst->list tree)
  (cond [(empty-tree? tree) '()]
        [(append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))]))

(define (bst? tree)
  (cond [(leaf? tree) #t]
        [(or (> (root tree) (root (right tree))) (< (root tree) (root (left tree)))) #f]
        [else (and (bst? (left tree)) (bst? (right tree)))]))