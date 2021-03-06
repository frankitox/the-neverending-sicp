(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (register-exp? exp)
  (tagged-list? exp 'reg))

(define (register-exp-reg exp)
  (cadr exp))

(define (constant-exp? exp)
  (tagged-list? exp 'const))

(define (constant-exp-value exp)
  (cadr exp))

(define (label-exp? exp)
  (tagged-list? exp 'label))

(define (label-exp-label exp)
  (cadr exp))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (per-instruction machine inst then)
  (if ((machine 'try-to-break!))
    (begin
      (display "STOPPED MACHINE") (newline)
      ((machine 'pause)))
    (begin
      ((machine 'count-instruction))
      (if ((machine 'tracing?))
        (begin
          (display "Executing instruction: ")
          (display inst)
          (newline)))
      (then))))
