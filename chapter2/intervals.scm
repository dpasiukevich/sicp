(define (make-interval a b) (if (< a b) (cons a b) (cons b a)))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i)) 

(define (make-center-percent center tolerance)
  (let ((width (* center tolerance 0.01)))
    (make-interval (- center width) (+ center width))))

(define (percent i) (/ (width i) (center i) 0.01))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0)) 

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y) 
  (make-interval (- (lower-bound x)  (upper-bound y)) 
		 (- (upper-bound x)  (lower-bound y)))) 
  
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4)))
  )

(define (div-interval x y)
  (if (<= 0 (* (lower-bound y)  (upper-bound y)))
    (error "Division error (interval spans 0)" y)
    (mul-interval x (make-interval (/ 1. (upper-bound y))
				   (/ 1. (lower-bound y)))))) 

(define (display-interval i)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define i (make-interval 2 5)) 
(define j (make-interval 3 4)) 

(define i+j (add-interval i j))
(define i-j (sub-interval i j))
(define i*j (mul-interval i j))
(define i/j (div-interval i j))
