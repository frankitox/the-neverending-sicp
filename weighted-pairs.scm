(load "merge-weighted.scm")
(load "stream-map.scm")

(define (weighted-pairs s1 s2 weight)
  (cons-stream (list (car s1) (car s2))
               (merge-weighted
                 (stream-map (lambda (n)
                               (list (car s1) n))
                             (stream-cdr s2))
                 (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
                 weight)))
