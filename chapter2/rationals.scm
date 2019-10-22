(define (add-rat x y)
  (make-rat (+ (* (numer x)  (denom y))
	       (* (numer y)  (denom x)))
	    (* (denom x)  (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x)  (denom y))
	       (* (numer y)  (denom x)))
	    (* (denom x)  (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x)  (numer y))
	    (* (denom x)  (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x)  (denom y))
	    (* (denom x)  (numer y))))

(define (equal-rat? x y)
  (= (* (numer x)  (denom y))
     (* (numer y)  (denom x))))

(define (numer x)  (car x))
(define (denom x)  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/") 
  (display (denom x)))

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

(define r (make-rat 15 -3))
(define r1 (make-rat 5 6))
(print-rat r)
(print-rat r1)
