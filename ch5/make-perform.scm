;; operation-exp?
;; make-operation-exp
;; advance-pc

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
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction:
                ASSEMBLE"
               inst))))
