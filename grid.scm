;;; grid.scm -- grid definitions for generating mazes

#lang racket

(provide make-empty-maze maze-ref maze-set! fold-maze!
         map-maze for-each-maze display-maze
         top? bottom? left? right?)

(define $side 20)

(define (make-empty-maze) (make-vector (* $side $side) 0))
(define (maze-ref maze i j) (vector-ref maze (+ (* $side i) j)))
(define (maze-set! maze i j v) (vector-set! maze (+ (* $side i) j) v))

(define (fold-maze! proc init maze)
  (let byrow ((row 0)
              (init init))
    (if (< row $side)
        (let bycol ((col 0)
                    (init init))
          (if (< col $side)
              (let ((next (proc maze row col init)))
                (bycol (+ col 1) next))
              (byrow (+ row 1) init)))
        maze)))

(define (map-maze proc maze)
  (let byrow ((row 0))
    (if (< row $side)
        (let bycol ((col 0))
          (if (< col $side)
              (let ((cell (proc maze row col)))
                (maze-set! maze row col cell)
                (bycol (+ col 1)))
              (byrow (+ row 1))))
        maze)))

(define (for-each-maze proc maze)
  (let byrow ((row 0))
    (if (< row $side)
        (let bycol ((col 0))
          (if (< col $side)
              (begin
                (proc maze row col)
                (bycol (+ col 1)))
              (byrow (+ row 1))))
        maze)))
  
(define (display-maze maze)
  (display (make-string (+ $side $side) #\_))
  (for-each-maze (lambda (m row col)
                   (if (= col 0) (newline) 1)
                   (let ((box (maze-ref maze row col)))
                     (cond ((= box 0) (display "_|"))
                           ((= box 1) (display "__"))
                           ((= box 2) (display " |"))
                           ((= box 3) (display "  "))
                           (else (error "unknown maze contents " maze row col box)))))
                 maze)
  (newline))

(define (top? maze row col) (= row 0))
(define (bottom? maze row col) (= row (- $side 1)))
(define (left? maze row col) (= col 0))
(define (right? maze row col) (= col (- $side 1)))
