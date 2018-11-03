(load "make-new-machine.scm")

(define (make-label
         inst machine labels operations pc)
  (lambda ()
    (if ((machine 'tracing?))
      (begin (display "Seen label: ")
             (display (cadr inst))
             (newline)))
    (advance-pc pc)))
