(load "stack-inst-reg-name.scm")
(load "make-register.scm")
(load "make-utils.scm")
(load "make-stack.scm")
(load "make-dict.scm")
(load "make-new-machine.scm")

(define (make-save inst machine dict pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (track-stack! machine reg)
    (lambda ()
      (per-instruction machine inst)
      (let ((stack (get dict (get-name reg))))
        (push stack (get-contents reg)))
      (advance-pc pc))))
