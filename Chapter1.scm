;; This might get moved to a literate org-mode file



;; 1.2 Translate into prefix form:
(/
 (+ 5 4
    (- 2
       (- 3
          (+ 6
             (/ 4 5)))))
 (* 3
    ( - 6 2)
    (- 2 7)))




;; 1.3 Define a procedure that takes three numbers as arguments and
;; returns the sum of the squares of the two larger numbers.


(define (sum-of-squares a b)
   (+ (* b b) (* a a) )
  )


(define (sum-of-squares-2-largest x y z)
  ;; Takes three numbers, returns the sum-of-squares
  ;; of the two largest.
  (cond ((and (>= x z) (>= y z)) (+ (* x x) (* y y) ))
        ((and (>= x y) (>= z y)) (+ (* x x) (* z z)))
        (else (+ (* y y) (* z z)))
        )
  )
;;Tests
(sum-of-squares-2-largest 2 3 4) 
(sum-of-squares-2-largest 4 2 3) 
(sum-of-squares-2-largest 3 4 2)


;; 1.4 Describe behavior of the following procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; if statement returns either a - or a +, which is then applied to the operands.



;; 1.5 How does this test if Scheme uses applicative order evaluation
;; or normal order evaluation?
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
; (test 1 (p))
;; Since this leads to an infinite loop as (test 0 (p)) is ever
;; expanded into itself, Scheme uses applicative order evaluation. If
;; it used normal order evaluation, the test would have returned 0
;; without expanding (p)



;; 1.6