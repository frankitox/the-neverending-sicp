(load "make-register.scm")
(load "make-dict.scm")
(load "make-stack.scm")

(define (add s elem)
  (if (member elem s)
    s
    (cons elem s)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (dict (make-dict))
        (instructions '())
        (entry-points '())
        (stack-regs '())
        (assignations '())
        (the-instruction-sequence '())
        (instruction-counter 0)
        (tracing false)
        (breakpoints '())
        (pause '())
        (just-breaked false))
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
                                      (car table-entry)
                                      (lambda (_) (make-stack))))
                                  register-table))))))
        (define (allocate-register name)
          (if (not (assoc name register-table))
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
                ((eq? message 'pause)
                 (lambda ()
                   (let ((pc-contents (get-contents pc)))
                     (set-contents! pc '())
                     (set! pause pc-contents)
                     (set! just-breaked true)
                     pc-contents)))
                ((eq? message 'try-to-break!)
                 (lambda ()
                   false))
                ((eq? message 'proceed)
                 (lambda ()
                   (set-contents! pc pause)
                   (set! pause '())
                   (execute)))
                ((eq? message 'breakpoints)
                 (lambda () breakpoints))
                ((eq? message 'set-breakpoint)
                 ;; lookup-label
                 (lambda (label n)
                   (if (assoc '(label n) breakpoints)
                     (display "There's already a breakpoint there")
                     (set! breakpoints (cons '((label n)) breakpoints)))))
                ((eq? message 'cancel-breakpoint)
                 (lambda (cancel-breakpoint label n)
                   (set! breakpoints (del-assoc '(label n) breakpoints))))
                ((eq? message 'cancel-all-breakpoints)
                 (lambda ()
                   (set! breakpoints '())))
                ((eq?
                   message
                   'install-instruction-sequence)
                 (lambda (seq)
                   (display "@@@@") (newline)
                   (display seq) (newline)
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
                ((eq? message 'add-instruction)
                 (lambda (inst)
                   (set! instructions
                     (sort
                       (add instructions inst)
                       (lambda (inst-1 inst-2)
                         (string<? (symbol->string (car inst-1))
                                   (symbol->string (car inst-2))))))))
                ((eq? message 'instructions) instructions)
                ((eq? message 'add-entry-point)
                 (lambda (reg)
                   (set!
                     entry-points
                     (add entry-points (get-name reg)))))
                ((eq? message 'entry-points) entry-points)
                ((eq? message 'save-or-restore)
                 (lambda (reg)
                   (set!
                     stack-regs
                     (add stack-regs (get-name reg)))))
                ((eq? message 'stack-regs) stack-regs)
                ((eq? message 'reset-instructions-counter)
                 (lambda ()
                   (set! instruction-counter 0)))
                ((eq? message 'count-instruction)
                 (lambda ()
                   (set! instruction-counter (+ 1 instruction-counter))))
                ((eq? message 'tracing?)
                 (lambda () tracing))
                ((eq? message 'trace-on)
                 (lambda () (set! tracing true)))
                ((eq? message 'trace-off)
                 (lambda () (set! tracing false)))
                ((eq? message 'statistics)
                 (lambda ()
                   (let ((dict-all (dict 'all)))
                     (display "Number of instructions executed: ")
                     (display instruction-counter) (newline)
                     (display "Final dictionary entries:") (newline)
                     (display
                       (map (lambda (stuff)
                              (let ((stack (cadr stuff)))
                                (list (car stuff) (stack 'all))))
                            dict-all))
                     (newline)
                     (for-each
                       (lambda (stuff)
                         (display "Stack statistics of register ")
                         (display (car stuff)) (display ":")
                         ((cadr stuff) 'print-statistics) (newline))
                       dict-all))))
                ((eq? message 'assignations)
                 (let ((assigns (make-dict)))
                   (assigns 'initialize)
                   (for-each
                     (lambda (inst)
                       (update assigns
                               (cadr inst)
                               (lambda (l)
                                 (cons (cddr inst) l))))
                     (filter (lambda (inst)
                               (eq? 'assign (car inst)))
                             instructions))
                   (assigns 'all)))
                ((eq? message 'operations)
                 the-ops)
                (else (error "Unknown request:
                             MACHINE"
                             message))))
      dispatch))))

(define (add-instruction! machine reg)
  ((machine 'add-instruction) reg))

(define (add-entry-point! machine reg)
  ((machine 'add-entry-point) reg))

(define (track-stack! machine reg)
  ((machine 'save-or-restore) reg))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (get-register machine reg-name)
  ((machine 'allocate-register) reg-name)
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

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  ((machine 'cancel-all-breakpoints)))

(define (proceed-machine machine)
  ((machine 'proceed)))

;; label1 0
;;   inst 1
;;   inst 2
;; label2 2
;;   inst 3
;;   inst 4
;; label2, 2
