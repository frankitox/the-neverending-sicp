(load "random-numbers.scm")
(load "map-successive-pairs.scm")

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))
