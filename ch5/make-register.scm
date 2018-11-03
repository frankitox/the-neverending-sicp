(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing true))
    (define (dispatch message)
      (cond ((eq? message 'name) name)
            ((eq? message 'get) contents)
            ((eq? message 'tracing?)
             (lambda () tracing))
            ((eq? message 'trace-on)
             (lambda () (set! tracing true)))
            ((eq? message 'trace-off)
             (lambda () (set! tracing false)))
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)))
            (else
              (error "Unknown request:
                     REGISTER"
                     message))))
      dispatch))

(define (get-name register)
  (register 'name))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))
