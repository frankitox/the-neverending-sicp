; Note: I thought that the first element of the
; stream should be just v0, but as I peeked others
; results they do it this way.
(load "integral.scm")
(load "add-streams.scm")
(load "scale-stream.scm")

(define (RC r c dt)
  (lambda (current v0)
    (add-streams
      (scale-stream current r)
      (scale-stream (integral (scale-stream current (/ 1.0 c)) v0 dt)))))
