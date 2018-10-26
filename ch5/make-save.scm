(load "stack-inst-reg-name.scm")
;; get-register
;; push
;; advance-pc

(define (make-save inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
