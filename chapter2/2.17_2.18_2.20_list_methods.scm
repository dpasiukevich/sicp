(define (last-pair l) (if (null? (cdr l)) l (last-pair (cdr l))))
(define (reverse l)
  (define (iter head res)
    (if (null? head)
      res
      (iter (cdr head) (cons (car head) res))))
  (iter l ()))

(define l (list 1 2 3 4))

(define (same-parity num . args)
  (define (iter head res parity)
    (cond ((null? head) res)
	  ((= (remainder (car head) 2) parity) (iter (cdr head) (cons (car head) res) parity))
	  (else (iter (cdr head) res parity))))
  (reverse (iter args (list num) (remainder num 2))))

; iterative
(define (square-list l)
  (define (square x) (* x x))
  (define (iter head res)
    (if (null? head) 
      res
      (iter (cdr head) (cons (square (car head)) res))))
  (reverse (iter l ())) )

(define (square-list-recur items)
  (define (iter l pick)
    (define r (square (car l)))
    (if (null? (cdr l))
      (pick (list r))
      (iter (cdr l) (lambda (x)  (pick (cons r x))))))
  (iter items (lambda (x) x))) 

(define (generate-list size)
  (define (iter count res)
    (if (= count 0)
      res
      (iter (- count 1) (cons (random 20) res))))
  (iter size ()))

;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2 3 4 5 6 7)
;(same-parity 2 4 6 7 9 10 12 11 15)
;(square-list (list 1 2 3 45))

;(generate-list 50)

(define (time-it nums func start-time annotation)
  (func nums)
  (display annotation) (display ": ") (display (- (runtime) start-time)) (newline))

(define test-list (generate-list 10000000))

;(time-it test-list square-list (runtime) "square-list")
;(time-it test-list square-list-recur (runtime) "square-list-recur")

(define (for-each func l)
  (cond ((null? l) #t)
	(else (func (car l)) 
	      (for-each func (cdr l)))))
