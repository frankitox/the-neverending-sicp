(define (memo-func fun)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! already-run? true)
                 (set! result (fun))
                 result)
          result))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (memo-func (lambda () b))))))
