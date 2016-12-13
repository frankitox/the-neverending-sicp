(load "ex-3.81.scm")
(load "stream-cdr.scm")
(load "scale-stream.scm")
(load "monte-carlo.scm")

(define (estimate-integral predicate x1 x2 y1 y2)
  (let ((x (- x2 x1))
        (y (- y2 y1)))
    (define (experiment random-stream)
      (cons-stream (list (+ (modulo (car random-stream) x) x1)
                         (+ (modulo (car (stream-cdr random-stream)) y) y1))
                   (experiment (stream-cdr (stream-cdr random-stream)))))
    (define trials
      (stream-map predicate
                  (experiment (random-numbers always-generate))))
    (scale-stream (monte-carlo trials 0.0 0)
                  (* x y))))

; (define pi (estimate-integral (lambda (pair) (<= (+ (expt (car pair) 2) (expt (car (cdr pair)) 2)) 1)) -1 1 -1 1))
