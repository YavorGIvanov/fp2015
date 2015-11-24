#lang racket

(require "tree.rkt")
(provide tree-levels)

(define (tree-level level tree)
  (cond [(empty-tree? tree) '()]
        [(<= level 1) (list (root tree))]
        [else (append (tree-level (- level 1) (left tree)) (tree-level (- level 1) (right tree)))]))

(define (tree-levels tree)
  (define (helper level)
    (cond [(> level (height t)) '()]
          [else (cons (tree-level level tree) (helper (+ level 1)))]))
  (helper 1))
  