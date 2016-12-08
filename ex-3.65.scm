(load "stream-map.scm")
(load "integers.scm")
(load "partial-sums.scm")

(define log2-series
  (partial-sums
    (stream-map (lambda (number)
                  (if (even? number)
                    (/ -1.0 number)
                    (/ 1.0 number)))
                integers)))
