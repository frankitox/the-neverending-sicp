(load "stack-inst-reg-name.scm")
(load "make-register.scm")
(load "make-stack.scm")
(load "make-new-machine.scm")

(define (make-save inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
