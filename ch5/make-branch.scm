(load "make-utils.scm")

;; TODO: Link to:
;; - lookup-label
;; - get-contents
;; - set-contents!
;; - advance-pc

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define
  (make-branch
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction:
                ASSEMBLE"
               inst))))