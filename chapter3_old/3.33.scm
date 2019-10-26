(load "constraint.scm")

(define (averager a b c)
  (let ((c1 (make-connector))
        (c2 (make-connector)))
    (constant 2 c1)
    (multiplier c c1 c2)
    (adder a b c2)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(averager A B C)
(set-value! A 6 'user)
(set-value! B 4 'user)
(probe "LUL" C)