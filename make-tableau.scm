(load "cons-stream.scm")

(define (make-tableau transform stream)
  (cons-stream stream
               (make-tableau transform (transform stream))))
