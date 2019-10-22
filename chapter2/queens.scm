(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
	safe?
	(append-map
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (iota board-size 1))) 
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? positions)
  (define (iter queens pos new-element)
    (cond ((null? queens) #t)
	  ((= (car queens) new-element) #f) ; same column
	  ((= pos (abs (- (car queens) new-element))) #f) ; same diagonal
	  (else (iter (cdr queens) (+ pos 1) new-element))))
  (iter (cdr positions) 1 (car positions)))

(queens 8)
(length (queens 10))
