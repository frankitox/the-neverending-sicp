(load "stream-map.scm")

(define (scale-stream stream factor)
  (stream-map (lambda(n) (* n factor))
              stream))
