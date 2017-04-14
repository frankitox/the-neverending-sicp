(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((let? exp) (eval (let->application exp) env))
        ((while? exp) (eval-while exp env))
        ((do? exp) (eval-do exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? exp) (null? (cdr exp)))

(define (first-operand exp) (car exp))

(define (rest-operands exp) (cdr exp))

(define (list-of-values exps env)
  (if (null? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exp env)
  (if (last-exp? exp)
      (eval (first-exp exp) env)
      (begin (eval (first-exp exp) env)
             (eval-sequence (rest-exps exp) env))))

(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (eval (assignment-value exp env))
    env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (and? exp) (tagged-list? exp 'and))

(define (aux-eval-and exp env)
  (let ((res (eval (first exp) env)))
    (cond ((eq? res #f) 'false)
          ((null? (cdr exp)) res)
          (else (eval-and (cdr exp) env)))))

(define (eval-and exp env)
  (if (null? (operands exp))
      'true
      (aux-eval-and (operands exp) env)))

(define (or? exp) (tagged-list? exp 'or))

(define (aux-eval-or exp env)
  (if (null? exp)
      'false
      (let ((res (eval (first exp) env)))
        (if (eq? res #f)
            (aux-eval-or (cdr exp) env)
            res))))

(define (eval-or exp env)
  (aux-eval-or (operands exp) env))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? exp) (null? (cdr exp)))

(define (first-exp exp) (car exp))

(define (rest-exps exp) (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq))
        ((last-exp? exp) (car seq))
        (else (make-begin seq)))

(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-arrow-clause? clause)
  (eq? '=> (cadr clause)))

(define (cond-actions clause)
  (if (eq? (cadr clause) '=>)
    (cddr clause)
    (cdr clause)))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
          (if (null? rest)
            (sequence->exp (cond-actions first))
            (error "else clause isn't last"))
          (make-if (cond-predicate first)
                   (if (cond-arrow-clause? first)
                       (cons (cond-actions first)
                             (cond-predicate first))
                       (sequence->exp
                         (cond-actions first)))
                   (expand-clauses rest))))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let->application exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda
            (map car bindings)
            (let-body exp))
          (map cadr bindings))))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-bindings exp) (cadr exp))

(define (let*-first-binding exp)
  (car (let*-bindings exp)))

(define (let*-drop-binding exp)
  (cons 'let
        (cons (cdr (cadr exp))
              (cddr exp))))

(define (let*->nested-lets exp)
  (if (null? (let*-bindings exp))
      (let-body exp)
      (make-let (list (let*-first-binding exp))
                (let*->nested-lets
                  (let*-drop-binding exp)))))

(define (while-predicate exp) (cadr exp))

(define (while-body exp) (cddr exp))

(define (while? exp) (tagged-list? exp 'while))

(define (eval-while exp env)
  (if (eval (while-predicate exp) env)
      (eval-while exp env)
      ()))

(define (do? exp) (tagged-list? exp 'do))

(define do-body cdr)

(define (eval-do exp env)
  (define (eval-do-inner body env)
    (if (and res (null? (cdr body)))
      res
      (eval-do-inner (cdr body) env)))
  (eval-do-inner (do-body exp)
                 (eval (car body) env)))

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define procedure-parameters cadr)
(define procedure-body caddr)
(define procedure-environment cadddr)

; frames
(define first-frame car)
(define make-frame cons)
(define frame-variables car)
(define frame-values cdr)
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

; enviroment
(define enclosing-environment cdr)
(define the-empty-environment ())
(define (extend-environment vars vals base-env)
  (if (eq? (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (error "More variables than values" vars vals)
      (error "More values than variables" vals vars))))
(define (lookup-variable-value var env)
  (define (scan vars vals)
    (if (null? vars)
      (lookup-variable-value
        var (enclosing-environment env))
      (if (eq? (car vars) var)
        (car vals)
        (scan (cdr vars) (cdr vals)))))
  (if (eq? the-empty-environment env)
    (error "Variable" var "not found.")
    (scan (frame-variables (first-frame env))
          (frame-values (first-frame env)))))
(define (set-variable-value! var val env)
  (define (scan vars vals)
    (if (null? vars)
      (set-variable-value!
        var val (enclosing-environment env))
      (if (eq? var (car vars))
        (set-car! vals val)
        (scan (cdr vars) (cdr vals)))))
  (if (eq? the-empty-environment env)
    (error "Variable" var "not found.")
    (scan (frame-variables (first-frame env))
          (frame-values (first-frame env)))))
(define (define-variable! var val env)
  (if (eq? the-empty-environment env)
    (error "The environment is empty")
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (if (null? vars)
          (add-binding-to-frame! var val frame)
          (if (eq? var (car vars))
            (set-car! vals val)
            (scan (cdr vars) (cdr vals)))))
      (scan
        (frame-variables frame)
        (frame-values frame)))))

(define (primitive-procedure? exp)
  (tagged-list? exp 'primitive))

(define (primitive-implementation exp)
  (cadr exp))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-values)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
           (primitive-procedure-names)
           (primitive-procedure-values)
           the-empty-environment)))
    (define-variable! 'false false initial-env)
    (define-variable! 'true true initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc)
    args))

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))
