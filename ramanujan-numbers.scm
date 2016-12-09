(load "weighted-pairs.scm")
(load "integers.scm")
(load "stream-cdr.scm")

(define (cubify pair)
  (+ (expt (car pair) 3)
     (expt (cadr pair) 3)))

(define ramanujan-ordered-pairs
  (weighted-pairs integers integers cubify))

(define (ramanujan-inner stream)
  (if (= (cubify (car stream)) (cubify (car (stream-cdr stream))))
    (cons-stream (list (car stream) (car (stream-cdr stream)))
                 (ramanujan-inner (stream-cdr (stream-cdr stream))))
    (ramanujan-inner (stream-cdr stream))))

(define ramanujan-numbers (ramanujan-inner ramanujan-ordered-pairs))
