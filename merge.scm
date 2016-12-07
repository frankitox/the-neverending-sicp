(load "stream-cdr.scm")

(define (merge s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((< (car s1) (car s2))
         (cons-stream (car s1)
                      (merge (stream-cdr s1) s2)))
        ((> (car s1) (car s2))
         (cons-stream (car s2)
                      (merge s1 (stream-cdr s2))))
        (else
          (cons-stream (car s1)
                       (merge (stream-cdr s1)
                              (stream-cdr s2))))))

; Ex 3.56
; (define s (merge (scale-stream integers 2)
;                  (merge (scale-stream integers 3)
;                         (scale-stream integers 5))))
