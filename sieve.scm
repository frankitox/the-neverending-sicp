(load "cons-stream.scm")
(load "stream-filter.scm")
(load "divisible.scm")
(load "stream-cdr.scm")

(define (sieve stream)
  (cons-stream (car stream)
               (sieve (stream-filter (lambda (n)
                                       (not (divisible? n (car stream))))
                                     (stream-cdr stream)))))

(load "integers-starting-from.scm")

(define primes (sieve (integers-starting-from 2)))
