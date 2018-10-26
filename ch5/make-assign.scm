(load "make-utils.scm")
(load "make-operation-exp.scm")
(load "make-primitive-exp.scm")

;; TODO: Link to:
;; - get-register
;; - operation-exp?
;; - make-operation-exp
;; - set-contents!
;; - advance-pc

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
        (set-contents! target (value-proc))
        (advance-pc pc)))))
