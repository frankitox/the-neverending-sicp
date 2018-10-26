(load "stack-inst-reg-name.scm")
(load "make-register.scm")
(load "make-stack.scm")
(load "make-new-machine.scm")

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
