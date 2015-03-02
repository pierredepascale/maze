;; maze.scm -- binary tree maze generation algorithm

#lang racket

(define $size 30)

(define (make-empty-maze) (make-vector (* $size $size) 0))
(define (maze-ref maze x y) (vector-ref maze (+ (* y $size) x)))
(define (maze-set! maze x y v) (vector-set! maze (+ (* y $size) x) v))

(define (show maze)
  (display "+--------------------------+")
  (newline)
  (let lpy ((y 0))
    (if (< y $size)
        (begin
          (let lpx ((x 0))
            (if (< x $size)
                (let ((c (maze-ref maze x y)))
                  (cond
                    ((= c 0) (display "_|"))
                    ((= c 1) (display "__"))
                    ((= c 2) (display " |")))
                  (lpx (+ x 1)))
              (newline)))
          (lpy (+ y 1)))
        maze)))

(define (gen maze)
  (let lp ((i 0))
    (if (< i (vector-length maze))
        (begin
          (cond ((< i $size) (vector-set! maze i 1))
                ((= (modulo i $size)
                    (- $size 1)) (vector-set! maze i 2))
                (else (vector-set! maze i (+ 1 (random 2)))))
          (lp (+ i 1)))
        maze)))
        
(define (test)
  (show (gen (make-empty-maze))))
