(load "stream-filter.scm")
(load "integers.scm")
(load "triples.scm")

(define square (lambda (x) (* x x)))

(define phytagorean-numbers
  (stream-filter
    (lambda (triple)
      (let ((a (car triple))
            (b (car (cdr triple)))
            (c (car (cdr (cdr triple)))))
        (= (+ (square a) (square b)) (square c))))
    (triples integers integers integers)))
