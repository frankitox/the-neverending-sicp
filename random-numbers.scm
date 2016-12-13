(load "basic-rand.scm")
(load "stream-map.scm")

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (random-numbers stream)
  (
