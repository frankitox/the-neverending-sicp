(load "mul-streams.scm")
(load "integers-starting-from.scm")

(define factorials
  (cons-stream 1
               (mul-streams factorials
                            (integers-starting-from 2))))
