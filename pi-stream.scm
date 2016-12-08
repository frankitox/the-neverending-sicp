(load "cons-stream.scm")

(define (pi-summands n)
  (let ((x (/ 1 (- (* n 2) 1))))
    (let ((y (if (even? n) (- x) x)))
      (cons-stream y
                   (pi-summands (+ n 1))))))

(load "partial-sums.scm")
(load "scale-stream")

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1.0)) 4))
