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

(define (pairs1 s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs1 (stream-cdr s) (stream-cdr t))))

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


(print-first-n (pairs1 integers integers) 10)