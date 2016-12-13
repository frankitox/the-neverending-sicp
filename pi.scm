(load "monte-carlo.scm")
(load "cesaro-stream.scm")
(load "stream-map.scm")

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))
