(load "stack-inst-reg-name.scm")
(load "make-register.scm")
(load "make-stack.scm")
(load "make-dict.scm")
(load "make-new-machine.scm")

(define (make-save inst machine dict pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (let ((stack (get dict (get-name reg))))
        (push stack (list
                      (get-name reg)
                      (get-contents reg))))
      (advance-pc pc))))
