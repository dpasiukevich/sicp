(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))

(define x (cons 'b '()))
(count-pairs (cons (cons 'a '()) (cons 'b '())))
(count-pairs (cons (cons 'a x) x))

(define rec (cons (cons 'a '()) (cons 'b '())))
(set-cdr! (cdr rec) rec)
(count-pairs rec)

(define x '(foo)) 
  (define y (cons x x)) 
  (define str2 (list y)) 
    (count-pairs str2)