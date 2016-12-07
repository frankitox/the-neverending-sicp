(load "cons-stream.scm")

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
