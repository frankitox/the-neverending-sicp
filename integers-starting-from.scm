(load "cons-stream.scm")

(define (integers-starting-from from)
  (cons-stream from
               (integers-starting-from (+ from 1))))
