(load "weighted-pairs.scm")
(load "integers.scm")
(load "stream-cdr.scm")

(define (squarify pair)
  (+ (expt (car pair) 2)
     (expt (cadr pair) 2)))

(define squared-ordered-pairs
  (weighted-pairs integers integers squarify))

(define (only-squary stream)
  (if (and (= (squarify (car stream))
              (squarify (car (stream-cdr stream))))
           (= (squarify (car (stream-cdr stream)))
              (squarify (car (stream-cdr (stream-cdr stream))))))
    (cons-stream (list (car stream)
                       (car (stream-cdr stream))
                       (car (stream-cdr (stream-cdr stream))))
                 (only-squary (stream-cdr (stream-cdr (stream-cdr stream)))))
    (only-squary (stream-cdr stream))))

(define squaries (only-squary squared-ordered-pairs))
