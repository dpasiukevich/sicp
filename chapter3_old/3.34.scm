(load "constraint.scm")

(define A (make-connector))
(define B (make-connector))

(define (squarer a b)
  (multiplier a a b))

(squarer A B)
(set-value! B 5 'user)
(probe "Square" A)