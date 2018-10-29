(load "make-register.scm")

(define (make-dict)
  (let ((d '()))
    (define (get key)
      (if (assoc key d)
        (cadr (assoc key d))
        (error "Dictionary key not found: GET " key)))
    (define (update key fn)
      (let ((val (assoc key d)))
        (set! d
              (cons
               (list key (fn (if val (cadr val) '())))
               (del-assoc key d)))))
    (define (initialize)
      (set! d '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'get) get)
            ((eq? message 'update) update)
            ((eq? message 'all) d)
            ((eq? message 'initialize)
             (initialize))
            (else
             (error "Unknown request: DICTIONARY"
                    message))))
    dispatch))

(define (get dict key)
  ((dict 'get) key))

(define (update dict key fn)
  ((dict 'update) key fn))
