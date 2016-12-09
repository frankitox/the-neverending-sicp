(load "stream-cdr.scm")

(define (merge-weighted s1 s2 weight)
  (let ((w1 (weight (car s1)))
        (w2 (weight (car s2))))
    (cond ((null? s1) s2)
          ((null? s2) s1)
          ((<= w1 w2)
           (cons-stream (car s1)
                        (merge-weighted (stream-cdr s1) s2 weight)))
          (else
           (cons-stream (car s2)
                        (merge-weighted s1 (stream-cdr s2) weight))))))
