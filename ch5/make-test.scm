(load "make-utils.scm")
(load "make-operation-exp.scm")
(load "make-register.scm")
(load "make-new-machine.scm")

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-test
         inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition
                machine
                labels
                operations)))
          (lambda ()
            (per-instruction machine inst)
            (set-contents!
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction:
                ASSEMBLE" inst))))
