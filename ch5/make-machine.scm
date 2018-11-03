(load "make-new-machine.scm")
(load "assemble.scm")

(define (make-machine ops
                      controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
