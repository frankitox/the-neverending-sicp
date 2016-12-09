(load "stream-cdr.scm")

(define (interleave s1 s2)
  (if (null? s1)
    s2
    (cons-stream (car s1)
                 (interleave s2 (stream-cdr s1)))))
