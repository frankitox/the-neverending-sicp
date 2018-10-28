(load "make-assign.scm")
(load "make-test.scm")
(load "make-branch.scm")
(load "make-goto.scm")
(load "make-save.scm")
(load "make-restore.scm")
(load "make-perform.scm")

(define (make-execution-procedure
         inst labels machine pc flag dict ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine dict pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine dict pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction
                      type: ASSEMBLE"
                     inst))))
