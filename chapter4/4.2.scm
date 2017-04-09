; a. It will interpret define as an operator

; b.
(define (application? exp) (tagged-list? exp 'call))
