(define (make-zero-crossings input-stream last-value fst-avg)
  (let ((snd-avg (/ (+ (car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector fst-avg snd-avg)
                 (make-zero-crossings (stream-cdr input-stream) (car input-stream) snd-avg))))
