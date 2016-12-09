(load "stream-map.scm")
(load "interleave.scm")

(define (pairs s1 s2)
  (cons-stream (list (car s1) (car s2))
               (interleave (stream-map (lambda (n)
                                         (list (car s1) n))
                                       (stream-cdr s2))
                           (pairs (stream-cdr s1) (stream-cdr s2)))))

; int-pairs is:
; (i, j) such that i <= j.

; (1, 1), (1, 2), (1, 3), (1, 4)
; (2, 1), (2, 2), (2, 3), (2, 4)
; (3, 1), (3, 2), (3, 3), (3, 4)
; (4, 1), (4, 2), (4, 3), (4, 4)

; 0       1       2       3       4       5
; 1       1       2       1       2       1
; (1, 1), (1, 2), (2, 2), (1, 3), (2, 3), (1, 4)

; +1 +98*2
; (0, 100)

; |
; (1, 100)
