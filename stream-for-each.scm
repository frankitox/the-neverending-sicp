(load "stream-cdr.scm")

(define (stream-for-each proc stream)
  (if (null? stream)
      'done
      (begin (proc (car stream))
             (stream-for-each proc (stream-cdr stream)))))
