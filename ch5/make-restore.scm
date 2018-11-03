(load "stack-inst-reg-name.scm")
(load "make-register.scm")
(load "make-stack.scm")
(load "make-dict.scm")
(load "make-new-machine.scm")

(define (get-tag-name value)
  (car value))

(define (get-tag-value value)
  (cadr value))

(define (make-restore inst machine dict pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (track-stack! machine reg)
      (lambda ()
        (let ((stack (get dict reg-name)))
         (let ((val (pop stack)))
           (begin
             ((machine 'count-instruction))
             (set-contents! reg val)
             (advance-pc pc))))))))
