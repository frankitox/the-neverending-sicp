(load "integral.scm")

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 (expt dt 2)))
  (define ddy (add-streams (stream-scale dy a)
                           (stream-scale y b)))
  y)
