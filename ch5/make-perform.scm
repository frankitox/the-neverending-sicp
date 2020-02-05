(load "make-operation-exp.scm")
(load "make-new-machine.scm")
(load "make-utils.scm")

(define (perform-action perform-instruction)
  (cdr perform-instruction))

(define (make-perform
          inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let ((action-proc
              (make-operation-exp
                action
                machine
                labels
                operations)))
        (lambda ()
          (per-instruction
            machine inst
            (lambda ()
              (action-proc)
              (advance-pc pc)))))
      (error "Bad PERFORM instruction:
             ASSEMBLE"
             inst))))
