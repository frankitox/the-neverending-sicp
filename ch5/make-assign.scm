(load "make-utils.scm")
(load "make-new-machine.scm")
(load "make-operation-exp.scm")
(load "make-primitive-exp.scm")
(load "make-register.scm")

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (make-assign
         inst machine labels operations pc)
  (let ((target
         (get-register
          machine
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure
                   ; for assign
        ((machine 'count-instruction))
        (set-contents! target (value-proc))
        (advance-pc pc)))))
