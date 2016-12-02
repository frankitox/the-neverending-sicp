(load "stream-cdr.scm")

(define (stream-ref stream n)
  (if (= n 0)
      (car stream)
      (stream-ref (stream-cdr stream) (- n 1))))
