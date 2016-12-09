; If you're looking for the position at which
; (k, q) will appear, it can be written as
; the position of (k, 0) which is 2^k-2
; plus the difference in position from (1, 2^k-1)
; to (k, q), which can be written as (1, 2^k-1) to
; (k, k + 1) being 2^(k-1) and from (k, k + 1)
; to (k, q) you have 2^k(q - (k + 1)).

; Summarizing:
;   2^k - 2 + 2^(k-1) + 2^k(q - (k + 1)).
