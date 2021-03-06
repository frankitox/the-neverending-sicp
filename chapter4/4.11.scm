; frame
(define first-frame car)

(define (make-frame vars vals) (map cons vars vals))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

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
  (define (scan frame)
    (if (null? frame)
      (set-variable-value!
        var val (enclosing-environment env))
      (if (eq? var (car (car frame)))
        (set-car! frame (cons var val))
        (scan (cdr frame)))))
  (if (eq? the-empty-environment env)
    (error "Variable" var "not found.")
    (let ((frame (first-frame env)))
      (scan frame))))

(define (define-variable! var val env)
  (if (eq? the-empty-environment env)
    (error "The environment is empty")
    (let ((the-frame (first-frame env)))
      (define (scan frame)
        (if (null? frame)
          (set-car! env (cons (cons var val) the-frame))
          (if (eq? var (car (car frame)))
            (set-car! frame (cons var val))
            (scan (cdr frame)))))
      (scan the-frame))))
