(load "stream-map.scm")

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda(n) (sqrt-improve n x)) guesses)))
  guesses)
