(load "integers-starting-from.scm")

(define integers (integers-starting-from 1))

; Alternatively:
; (define integers (cons-stream 1
;                               (add-streams ones integers)))
