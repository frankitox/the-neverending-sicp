(load "stream-ref.scm")
(load "stream-cdr.scm")

(define (euler-transform stream)
  (let ((s0 (stream-ref stream 0))
        (s1 (stream-ref stream 1))
        (s2 (stream-ref stream 2)))
    (cons-stream (- s2
                    (/ (* (- s2 s1) (- s2 s1))
                       (+ s0 (* s1 -2) s2)))
                 (euler-transform (stream-cdr stream)))))

; sn+1 - (sn+1 -sn)^2/(sn-1 - 2sn + sn+1)

; (define (minus-stream s1 s2)
;   (add-streams s1
;                (scale-streams s2 -1)))
;
;  (minus-streams
;    stream
;    (div-streams
;      (mul-streams
;        (minus-stream (stream-cdr (stream-cdr stream)) (stream-cdr stream))
;        (minus-stream (stream-cdr (stream-cdr stream)) (stream-cdr stream)))
;      (add-streams (minus-streams
;                     stream
;                     (scale-stream (stream-cdr stream) 2))
;                   (stream-cdr (stream-cdr stream)))
