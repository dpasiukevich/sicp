(define (equal1? l1 l2)
  (if (and (pair? l1) (pair? l2))
    (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))
    (eq? l1 l2)))

(equal1? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)) 
(equal1? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)) 
(equal1? (list 7) ())
(equal1? '(1 2 3) '(1 2 3 4)) 
