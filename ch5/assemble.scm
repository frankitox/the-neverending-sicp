(load "make-utils.scm")
(load "make-execution-procedure.scm")

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc!
         inst
         proc)
  (set-cdr! inst proc))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (dict (machine 'dict))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels
         machine
         pc
         flag
         dict
         ops)))
     insts)))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (if (assoc next-inst labels) ;; Ex5.8: Fix using the last label,
              ;; instead signal an error.
              (error "Duplicated label: ASSEMBLE"
                     label-name)
              (receive
                (cons (make-instruction
                        (list 'label next-inst))
                      insts)
                (cons
                  (make-label-entry
                    next-inst
                    insts)
                  labels)))
            (receive
              (cons (make-instruction
                      next-inst)
                    insts)
              labels)))))))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))
