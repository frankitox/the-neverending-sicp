(load "integrate-series.scm")
(load "scale-stream.scm")

(define cosine-series
  (cons-stream 1
               (integrate-series
                 (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))
