(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))
      )
    )
  (iter a null-value)
  )

(define (filtered-accumulate filter-func combiner-func null-value term-func a next b)
  (define (update-result term result)
    (if (filter-func term) 
      (combiner-func term result)
      result))
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (update-result (term-func a) result)))
    )
  (iter a null-value)
  )

; O(n) space 
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b)
       )
    )
  )

; O(1) space
(define (sum term a next b) (accumulate + 0 term a next b))

; O(1) space
(define (product term a next b) (accumulate * 1 term a next b))

(define (product-recur term a next b)
  (if (> a b)
    1
    (* (term a) (product-recur term (next a) next b))
    )
  )

(define (factorial n)
  (product identity 1 inc n))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b)
  )

(define (sum-integers a b)
  (sum identity a inc b)
  )

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b)
  )

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx)
  )

