(load "stream-map.scm")
(load "integers.scm")
(load "mul-streams.scm")

(define (integrate-series serie)
  (let ((harmonic-series (stream-map (lambda (n) (/ 1 n)) integers)))
    (mul-streams harmonic-series serie)))
