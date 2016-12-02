(load "stream-cdr.scm")

(define (stream-filter pred stream)
  (cond ((null? stream) '())
        ((pred (car stream))
          (cons-stream (car stream)
                       (stream-filter pred
                                      (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))
