(load "interleave.scm")
(load "pairs.scm")

(define (triples s1 s2 s3)
  (cons-stream (list (car s1) (car s2) (car s3))
               (interleave (stream-map (lambda (pair)
                                         (cons (car s1) pair))
                                       (pairs s2 (stream-cdr s3)))
                           (triples (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))))
