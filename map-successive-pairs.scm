(load "stream-cdr.scm")

(define (map-successive-pairs f stream)
  (cons-stream
    (f (car stream) (car (stream-cdr stream)))
    (map-successive-pairs f (stream-cdr (stream-cdr stream)))))
