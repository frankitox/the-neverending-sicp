(vector-ref ⟨vector⟩ ⟨n⟩)
(vector-set! ⟨vector⟩ ⟨n⟩ ⟨value⟩)


(assign ⟨reg₁⟩ (op car) (reg ⟨reg₂⟩))
(assign ⟨reg₁⟩ (op cdr) (reg ⟨reg₂⟩))

(assign ⟨reg₁⟩ (op vector-ref) (reg the-cars) (reg ⟨reg₂⟩))
(assign ⟨reg₁⟩ (op vector-ref) (reg the-cdrs) (reg ⟨reg₂⟩))


(perform (op set-car!) (reg ⟨reg₁⟩) (reg ⟨reg₂⟩))
(perform (op set-cdr!) (reg ⟨reg₁⟩) (reg ⟨reg₂⟩))

(perform (op vector-set!) (reg the-cars) (reg ⟨reg₁⟩) (reg ⟨reg₂⟩))
(perform (op vector-set!) (reg the-cdrs) (reg ⟨reg₁⟩) (reg ⟨reg₂⟩))


(assign ⟨reg₁⟩ (op cons) (reg ⟨reg₂⟩) (reg ⟨reg₃⟩))

(perform (op vector-set!) (reg the-cars) (reg free) (reg ⟨reg₂⟩))
(perform (op vector-set!) (reg the-cdrs) (reg free) (reg ⟨reg₃⟩))
(assign ⟨reg₁⟩ (reg free))
(assign free (op +) (const 1) (reg free))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else
          (+ (count-leaves (car tree))
             (count-leaves (cdr tree))))))

start
(assign continue (label end))
(save end)
fun
(test (op null?) (reg tree))
(branch (label null))
(test (op pair?) (reg tree))
(branch (label pair-1))
not-pair
(assign val (const 1))
(restore continue)
(goto (reg continue))
null
(assign val (const 0))
(restore continue)
(goto (reg continue))
pair-1
(save (label pair-2))
(save tree)
(assign tree (op car) (reg tree))
(goto (label fun))
pair-2
(save (label pair-sum))
(save tree)
(save val)
(assign tree (op cdr) (reg tree))
(goto (label fun))
pair-sum
(restore aux) ; restores last val
(restore tree)
(assign val (op +) (reg aux) (reg val))
end
