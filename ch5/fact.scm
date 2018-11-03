(load "make-new-machine.scm")
(load "make-machine.scm")

(define fact-controller
  '((perform (op print) (const "Compute factorial of: "))
    (assign n (op read))
    (assign continue (label fact-done))   ; set up final return address
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    (save continue)                       ; Set up for the recursive call
    (save n)                              ; by saving n and continue.
    (assign n (op -) (reg n) (const 1))   ; Set up continue so that the
    (assign continue (label after-fact))  ; computation will continue
    (goto (label fact-loop))              ; at after-fact when the
    after-fact                            ; subroutine returns.
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val)) ; val now contains n(n - 1)!
    (goto (reg continue))                 ; return to caller
    base-case
    (assign val (const 1))                ; base case: 1! = 1
    (goto (reg continue))                 ; return to caller
    fact-done))

(define (call-machine controller ops)
  (let ((machine (make-machine ops
                               (cons '(perform (op initialize-dict))
                                     controller))))
    (start machine)
    machine))

(define (fact)
  (let ((machine
          (call-machine fact-controller (list (list '= =)
                                              (list '- -)
                                              (list 'read read)
                                              (list 'print display)
                                              (list '* *)))))
    ((machine 'statistics))
    machine))
