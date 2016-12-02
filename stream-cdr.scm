(load "cons-stream.scm")

(define (force thunk)
  (thunk))

(define (stream-cdr stream)
  (force (cdr stream)))
