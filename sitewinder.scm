;;; sitewinder.scm -- sitewinder algorithm for maze generation

#lang racket

(define $width 20)

(define (make-empty-maze) (make-vector (* $width $width) 0))
(define (maze-ref maze i j) (vector-ref maze (+ (* $width i) j)))
(define (maze-set! maze i j v) (vector-set! maze (+ (* $width i) j) v))
(define (fold-maze! proc init maze)
  (let byrow ((row 0)
              (init init))
    (if (< row $width)
        (let bycol ((col 0)
                    (init init))
          (if (< col $width)
              (let ((next (proc maze row col init)))
                (bycol (+ col 1) next))
              (byrow (+ row 1) init)))
        maze)))

(define (sitewinder maze)
  (let lp ((row 0))
    (if (< row $width)
        (let bycell ((col 0)
                     (run '()))
          ;(display (list 'doing row col run)) (newline)
            (cond ;((and (= col $width) (= row $width)) maze)
                  ((= col $width)
                   (if (pair? run) (open-north! maze run) 1)
                   (lp (+ row 1)))
                  ((at-bottom? row col)
                   (if (= 0 (random 2))
                       (begin
                         (open-east! maze row col)
                         (bycell (+ col 1) (cons (cons row col) run)))
                       (bycell (+ col 1) '())))
                  ((= 0 (random 2))
                   (open-east! maze row col)
                   (bycell (+ col 1) (cons (cons row col) run)))
                  (else
                   (open-north! maze (cons (cons row col) run))
                   (bycell (+ col 1) '()))))
          maze)))

(define (at-east? row col) (= col $width))
(define (at-bottom? row col) (= row $width))

(define (open-north! maze run)
  (let ((cell (list-ref run (random (length run)))))
    (maze-set! maze (car cell) (cdr cell) (+ (maze-ref maze (car cell) (cdr cell)) 2))))

(define (open-east! maze row col)
  (maze-set! maze row col 1))

(define (display-maze maze)
  (display "+--------------+") (newline)
  (fold-maze! (lambda (m row col i)
                (if (= col 0) (newline) 1)
                (let ((box (maze-ref maze row col)))
                  (cond ((= box 0) (display "_|"))
                        ((= box 1) (display "__"))
                        ((= box 2) (display " |"))
                        ((= box 3) (display "  "))
                        (else (error "unknown maze contents " maze row col box)))))
              #f maze)
  #f)

(define (test) (display-maze (sitewinder (make-empty-maze))))

(define m (make-empty-maze))
