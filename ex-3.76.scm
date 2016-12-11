(define (smooth stream)
  (cons-stream (/ (car stream) (car (stream-cdr stream)))
               (smooth (stream-cdr stream))))

(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector (smooth (input-stream))))
