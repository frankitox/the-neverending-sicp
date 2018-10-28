(load "make-register.scm")

(define (make-dict)
  (let ((d '()))
    (define (get key)
      (if (assoc key d)
        (cadr (assoc key d))
        (error "Dictionary key not found: GET " key)))
    (define (update register value)
      (let ((key (get-name register)))
        (set! d
              (cons
               (del-assoc key d)
               (list key value)))))
    (define (initialize)
      (set! d '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'get) get)
            ((eq? message 'update) update)
            ((eq? message 'initialize)
             (initialize))
            (else
             (error "Unknown request: DICTIONARY"
                    message))))
    dispatch))

(define (get dict key)
  ((dict 'get) key))

(define (update dict register value)
  ((dict 'update) register value))
