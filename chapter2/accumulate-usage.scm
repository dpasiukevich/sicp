(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (el)
			 (cond ((null? el) 0)
			       ((pair? el) (count-leaves el))
			       (else 1))) t)))

(define x (cons (list 1 2) (list 3 4)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map car seqs))
	  (accumulate-n op init (map cdr seqs)))))

;(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

;(reverse-fold-right (list 1 2 3 4))
;(reverse-fold-left (list 1 2 3 4))
