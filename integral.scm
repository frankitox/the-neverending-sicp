(load "add-streams.scm")
(load "scale-stream.scm")

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (delayed-integrand)))
                 (if (null? integrand)
                   '()
                   (integral (lambda() (stream-cdr integrand))
                             (+ (* dt (car integrand))
                                initial-value)
                             dt)))))
