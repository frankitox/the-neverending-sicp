(load "stack-inst-reg-name.scm")
(load "make-register.scm")
(load "make-stack.scm")
(load "make-new-machine.scm")

(define (get-tag-name value)
  (car value))

(define (get-tag-value value)
  (cadr value))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (track-stack! machine reg)
      (lambda ()
        (let ((tagged-val (pop stack)))
          (if (eq? reg-name (get-tag-name tagged-val))
            (begin
              (set-contents! reg (get-tag-value tagged-val))
              (advance-pc pc))
            (error "Bad RESTORE: Can't pop " reg-name)))))))
