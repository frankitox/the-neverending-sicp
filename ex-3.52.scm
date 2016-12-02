(load "stream-map.scm")
(load "stream-enumerate-interval.scm")
(load "stream-filter.scm")
(load "stream-ref.scm")
(load "display-stream.scm")

(define sum 0)

(define (accum x)
  (set! sum (+ sum x))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; (= sum 1)
; seq will become (1, 3, 6, 10, 15, 21, 28, 36,
;                  45, 55, 66, 78, 91, 105, 120,
;                  136, 153, 171, 190, 210)

(define y (stream-filter even? seq))
; (= sum (+ 1 2 3))
; y is (cons 6 (stream-map accum (stream-enumerate-interval 4 20))))

(define z (stream-filter (lambda(x) (= (remainder x 5) 0))
                         seq))
; (= sum (+ 1 2 3 4))
; z is (cons 10 (stream-filter (lambda(x) (= (remainder x 5) 0))
;                              (stream-cdr⁴ seq)))

(stream-ref y 7)
; (= sum (+ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
; (= sum 136)

(display-stream z)
; (= sum (+ 1 2 3 4 5 6 7 8 ... 20))
; (= sum 210)
