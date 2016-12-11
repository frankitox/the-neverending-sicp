; This one has been copied straight from
; http://community.schemewiki.org/?sicp-ex-3.79

(define (general-solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)
