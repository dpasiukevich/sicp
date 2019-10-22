(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (define (average x y) (/ (+ x y) 2.0))
  (let ((start (start-segment s))
	(end (end-segment s)))
    (make-point (average (x-point start)
			 (x-point end))
		(average (y-point start)
			 (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rectangle p1 p2) (cons p1 p2))

(define (rectangle-p1 r) (car r))
(define (rectangle-p2 r) (cdr r))

(define (rectangle-len1 r) 
  (let ((p1 (rectangle-p1 r))
	(p2 (rectangle-p2 r)))
    (abs (- (x-point p1) (x-point p2)))))

(define (rectangle-len2 r) 
  (let ((p1 (rectangle-p1 r))
	(p2 (rectangle-p2 r)))
    (abs (- (y-point p1) (y-point p2)))))

(define (rectangle-perimeter r) 
  (* 2 (+ (rectangle-len1 r) (rectangle-len2 r))))

(define (rectangle-area r) 
  (* (rectangle-len1 r) (rectangle-len2 r)))
