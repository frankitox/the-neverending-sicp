(load "stream-cdr.scm")

(define (stream-map proc . args-stream)
  (if (null? (car args-stream))
      '()
      (cons-stream
        (apply proc (map car args-stream))
        (apply stream-map
               (cons proc (map stream-cdr args-stream))))))
