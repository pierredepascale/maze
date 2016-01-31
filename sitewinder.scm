;;; sitewinder.scm -- sitewinder algorithm for maze generation

;#lang racket

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

(define (open-north! maze run)
  (let ((cell (list-ref run (random (length run)))))
    (maze-set! maze (car cell) (cdr cell) (+ (maze-ref maze (car cell) (cdr cell)) 2))))

(define (open-east! maze row col)
  (maze-set! maze row col 1))

(define (test-sitewinder) (display-maze (sitewinder (make-empty-maze))))

(define m (make-empty-maze))
