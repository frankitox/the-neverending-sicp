(load "make-utils.scm")
(load "make-register.scm")

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                   (lookup-label
                     labels
                     (label-exp-label dest))))
             (lambda ()
               (per-instruction
                 machine inst
                 (lambda ()
                   (set-contents! pc insts))))))
          ((register-exp? dest)
           (let ((reg
                   (get-register
                     machine
                     (register-exp-reg dest))))
             (add-entry-point! machine reg)
             (lambda ()
               (per-instruction
                 machine inst
                 (lambda ()
                   (set-contents! pc (get-contents reg)))))))
          (else (error "Bad GOTO instruction:
                       ASSEMBLE"
                       inst)))))
