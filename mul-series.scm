(define (mul-series s1 s2)
  (cons-stream (* (car s1) (car s2))
               (add-streams (scale-stream (stream-cdr s1) (car s2))
                            (mul-series (stream-cdr s2) s1))))
