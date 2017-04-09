; Just transform them in a series of nested if's.

(define (and->if- exp)
  (if (null? (cdr exp))
      (car exp)
      (make-if (first exp)
               (and->if- (cdr exp))
               'false)))

(define (and->if exp)
  (let ((o (operands exp)))
    (if (null? o)
        'false
        (and->if- o))))

(define (or->if- exp)
  (if (null? exp)
      'false
      (make-if (first exp)
               'true
               (or->if- (cdr exp)))))

(define (or->if exp)
  (or->if- (operands exp)))
