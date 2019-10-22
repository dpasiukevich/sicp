(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (has-cycle? input)
  (define visited '())
  (define (iter x)
    (cond ((or (null? x) (not (pair? x))) #f)
          ((memq x visited) #t)
          (else (set! visited (cons x visited))
                (iter (cdr x)))))
  (iter input))

(define x (cons 'b '()))
(has-cycle? (cons (cons 'a x) x))

(has-cycle? (make-cycle '(a b c (a s))))

(define t1 (cons 'a 'b)) 
  (define t2 (cons t1 t1)) 
    (has-cycle? t2)

    (define x '(a b c)) 
      (define y '(d e f)) 
      (set-car! (cdr x) y) 
      (set-car! x (cdr x)) 
      (set-cdr! (last-pair y) (cdr y)) 
        (has-cycle? y)