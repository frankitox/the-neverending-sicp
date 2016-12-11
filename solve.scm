(load "integral.scm")
(load "stream-map.scm")

(define (solve f y0 dt)
  (define y (integral (lambda() dy) y0 dt))
  (define dy (stream-map f y))
  y)
