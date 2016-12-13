(load "basic-rand.scm")
(load "stream-cdr.scm")
(load "stream-map.scm")

; operations is a stream of 'generate and ('reset k) elements.
(define (random-numbers operations)
  (let ((initial-number (if (eq? (car operations) 'generate)
                          random-init
                          (car (cdr (car operations))))))
    (define numbers
      (cons-stream initial-number
                   (stream-map (lambda (old-number operation)
                                 (if (eq? operation 'generate)
                                   (rand-update old-number)
                                   (car (cdr operation))))
                               numbers
                               (stream-cdr operations))))
    numbers))

(define always-generate
  (cons-stream 'generate always-generate))

(define some-resets
  (cons-stream (list 'reset 10)
               (cons-stream 'generate
                            (cons-stream (list 'reset 201)
                                         always-generate))))

; (random-numbers always-generate)
; (random-numbers some-resets)
