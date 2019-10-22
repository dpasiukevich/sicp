(define (deep-reverse l) 
  (map (lambda (el) (if (pair? el) (deep-reverse el) el)) (reverse l)))

(define (fringe tree)
  (define (iter sub-tree result)
    (if (null? sub-tree)
      result
      (iter (cdr sub-tree) 
	    (append result (if (pair? (car sub-tree))
			     (fringe (car sub-tree))
			     (list (car sub-tree)))))))
  (iter tree ()))

