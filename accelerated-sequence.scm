(load "make-tableau.scm")

(define (accelerated-sequence transform stream)
  (stream-map car
              (make-tableau transform stream)))
