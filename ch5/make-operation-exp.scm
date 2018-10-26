(load "make-utils.scm")
(load "make-primitive-exp.scm")

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                ;; Exercise 5.9: Allow only registers and constants.
                (if (or (register-exp? e)
                        (label-exp? e))
                  (make-primitive-exp
                   e machine labels)
                  (error "Unknown operation parameter:
                          ASSEMBLE"
                         e)))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))
