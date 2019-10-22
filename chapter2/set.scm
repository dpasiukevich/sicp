(define (subsets s)
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (el) (cons (car s) el)) rest)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set))))) 

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set))) 

(define (union-set s1 s2)
  (append s2
	  (filter (lambda (el) (not (element-of-set? el s2)))
		  s1)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1)  (null? set2)) ' ())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;;;;;;;;;;;;;;
; sets as list with duplicates
(define (element-of-set-dup? el set) 
  (not (null? (memq el set))))

(define (adjoin-set-dup el set) (cons el set))

(define (union-set-dup s1 s2) (append s1 s2))

(define (intersection-set-dup s1 s2)
  (filter (lambda (el) (element-of-set-dup? el s2))
	  s1))
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))  (x2 (car set2)))
      (cond ((= x1 x2)
	     (cons x1
		   (intersection-set-ordered (cdr set1)
					     (cdr set2))))
	    ((< x1 x2)
	     (intersection-set-ordered (cdr set1) set2))
	    ((< x2 x1)
	     (intersection-set-ordered set1 (cdr set2)))))))

(define (adjoin-set-ordered el set)
  (cond ((or (null? set) (< el set)) (cons el set))
	((= el (car set)) set)
	(else (cons (car set) (adjoin-set-ordered el (cdr set))))))

(define (union-set-ordered s1 s2)
  (if (or (null? s1) (null? s2))
    (append s1 s2)
    (let ((el1 (car s1))
	  (el2 (car s2)))
      (cond ((= el1 el2) (cons el1 (union-set (cdr s1) (cdr s2))))
	    ((< el1 el2) (cons el1 (union-set (cdr s1) s2)))
	    (else (cons el2 (union-set s1 (cdr s2))))))))

;;;;;;;;;;;;;;
; set as binary tree

(define (entry tree)  (car tree))
(define (left-branch tree)  (cadr tree))
(define (right-branch tree)  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set)  (make-tree x '() ' ()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
		    (cons (entry tree)
			  (copy-to-list (right-branch tree)
					result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
	(let ((left-tree (car left-result))
	      (non-left-elts (cdr left-result))
	      (right-size (- n (+ left-size 1))))
	  (let ((this-entry (car non-left-elts))
		(right-result (partial-tree (cdr non-left-elts)
					    right-size)))
	    (let ((right-tree (car right-result))
		  (remaining-elts (cdr right-result)))
	      (cons (make-tree this-entry left-tree right-tree)
		    remaining-elts))))))))

(define (union-set-tree s1 s2)
  (list->tree (union-set-ordered (tree->list s1) (tree->list s2))))

(define (intersection-set-tree s1 s2)
  (list->tree (intersection-set-ordered (tree->list s1) (tree->list s2))))

; tree lookup of value by key
(define (lookup key tree)
  (cond ((null? tree) (error "key error"))
	(let ((node-key (key (entry tree))))
	  (cond ((= key node-key) (entry tree))
		((< key node-key) (lookup key (left-branch tree)))
		(else (lookup key (right-branch tree)))))))
