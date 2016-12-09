(load "stream-map.scm")
(load "interleave.scm")

(define (pairs s1 s2)
  (cons-stream (list (car s1) (car s2))
               (interleave (stream-map (lambda (n)
                                         (list (car s1) n))
                                       (stream-cdr s2))
                           (interleave (stream-map (lambda (n)
                                                     (list n (car s2)))
                                                   (stream-cdr s1))
                                       (pairs (stream-cdr s1) (stream-cdr s2))))))


