(load "stream-filter.scm")
(load "integers-starting-from.scm")

(define primes
  (cons-stream 2
               (stream-filter prime?
                              (integers-starting-from 3))))

(load "divisible.scm")

(define (prime? integer)
  (define (iter primes)
    (cond ((> (square (car primes)) integer)
           true)
          ((divisible? integer (car primes))
           false)
          (else
            (iter (stream-cdr primes)))))
  (iter primes))
