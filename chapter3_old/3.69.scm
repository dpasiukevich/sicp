(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (print-first-n stream n)
  (cond ((> n 0)
         (display (stream-car stream))
         (newline)
         (print-first-n (stream-cdr stream) (- n 1)))))

(define (triples s t k)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car k))
    (interleave
      (stream-map (lambda (x) (append (list (stream-car s)) x))
                  (stream-cdr (pairs t k)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr k)))))

(define pythagorean-triples
  (stream-filter (lambda (t)
                   (= (+ (square (car t))
                         (square (cadr t)))
                      (square (caddr t))))
                 (triples integers integers integers) ))
 

(print-first-n pythagorean-triples 5)