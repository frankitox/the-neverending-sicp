; context
(define first-frame car)
(define make-frame cons)
(define frame-variables car)
(define frame-values cdr)
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))
(define enclosing-environment cdr)
(define the-empty-environment ())
(define (extend-environment vars vals base-env)
  (if (eq? (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (error "More variables than values" vars vals)
      (error "More values than variables" vals vars))))

; Exercise
(define (scan-frame var env found not-found)
  (define (scan vars vals)
    (cond ((null? vars) (not-found))
          ((eq? var (car vars)) (found vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? the-empty-environment env)
    (error "Variable" var "not found.")
    (let ((frame (first-frame env)))
      (scan (frame-variables frame)
            (frame-values frame)))))

(define (lookup-variable-value var env)
  (scan-frame var env
              (lambda (vals) (car vals))
              (lambda () (error "Variable" var "not found."))))

(define (set-variable-value! var val env)
  (if (eq? the-empty-environment env)
    (error "Variable" var "not found.")
    (scan-frame
      var env
      (lambda (vals) (set-car! vals val))
      (lambda ()
        (set-variable-value!
          var val (enclosing-environment env))))))

(define (define-variable! var val env)
  (scan-frame
    var
    env
    (lambda (vals)
      (set-car! vals val))
    (lambda ()
      (add-binding-to-frame! var val (first-frame env)))))
