(load "stack-inst-reg-name.scm")
;; get-register
;; pop
;; set-contents!
;; advance-pc

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
