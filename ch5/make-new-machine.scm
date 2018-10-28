(load "make-register.scm")
(load "make-dictionary.scm")
(load "make-stack.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (dict (make-dict))
        (the-instruction-sequence '()))
    (let ((register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (let ((the-ops
             (list
              (list 'initialize-dict
                    (lambda ()
                      (dict 'initialize)
                      (for-each (lambda (table-entry)
                                  (update
                                   dict
                                   (cadr table-entry)
                                   (make-stack)))
                                register-table))))))
        (define (allocate-register name)
          (if (assoc name register-table)
              (error
               "Multiply defined register: "
               name)
              (set! register-table
                    (cons
                     (list name
                           (make-register name))
                     register-table)))
          'register-allocated)
        (define (lookup-register name)
          (let ((val
                 (assoc name register-table)))
            (if val
                (cadr val)
                (error "Unknown register:"
                       name))))
        (define (execute)
          (let ((insts (get-contents pc)))
            (if (null? insts)
                'done
                (begin
                  ((instruction-execution-proc
                    (car insts)))
                  (execute)))))
        (define (dispatch message)
          (cond ((eq? message 'start)
                 (set-contents!
                  pc
                  the-instruction-sequence)
                 (execute))
                ((eq?
                  message
                  'install-instruction-sequence)
                 (lambda (seq)
                   (set!
                    the-instruction-sequence
                    seq)))
                ((eq? message
                      'allocate-register)
                 allocate-register)
                ((eq? message 'get-register)
                 lookup-register)
                ((eq? message
                      'install-operations)
                 (lambda (ops)
                   (set! the-ops
                         (append the-ops ops))))
                ((eq? message 'dict) dict)
                ((eq? message 'operations)
                 the-ops)
                (else (error "Unknown request:
                              MACHINE"
                             message))))
        dispatch))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (start machine)
  (machine 'start))

(define (get-register-contents
         machine register-name)
  (get-contents
   (get-register machine register-name)))

(define (set-register-contents!
         machine register-name value)
  (set-contents!
   (get-register machine register-name)
   value)
  'done)
