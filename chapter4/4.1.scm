; I'm not sure if this works, plus everybody did
; it with let.
; https://wizardbook.wordpress.com/2010/12/23/exercise-4-1/

(define (list-of-values* exps computed env)
  (if (no-operands? exps)
      computed
      (list-of-values* (rest-operands exps)
                       (cons (eval (first-operand exps)) computed)
                       env)))

; left to right.
(define (list-of-values exps env)
  (list-of-values* exps '() env))

; right to left.
(define (list-of-values exps env)
  (list-of-values* (reverse exps) '() env))
