;; TODO: Link to:
;; - operation-exp?
;; - make-operation-exp
;; - set-contents!
;; - advance-pc

(define (test-condition test-instruction)
  (cdr test-instruction))

(define
  (make-test
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition
                machine
                labels
                operations)))
          (lambda ()
            (set-contents!
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction:
                ASSEMBLE" inst))))