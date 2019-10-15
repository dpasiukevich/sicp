(load "fixed_point.scm")

; no average dumping
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (+ (/ (log 1000) (* 2 (log x)))
			    (/ x 2))
	       ) 2.0)
