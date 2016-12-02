(load "stream-for-each.scm")
(load "display-line.scm")

(define (display-stream stream)
  (stream-for-each display-line stream))
