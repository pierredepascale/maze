;; binary-tree.scm -- binary tree maze generation algorithm

#lang racket

(require "grid.scm")

(define (binary-tree maze)
  (map-maze (lambda (maze row col)
              (cond
                ((and (right? maze row col) (bottom? maze row col)) 0)
                ;; if we are in the right column, we open only below
                ((right? maze row col) 2)
                ;; if we are in the top row, we always open on the right
                ((bottom? maze row col) 1)
                ;; otherwise open either right or below randomly
                (else (+ 1 (random 2)))))
            maze))
                                                                
(define (test-binary-tree)
  (display-maze (binary-tree (make-empty-maze))))
