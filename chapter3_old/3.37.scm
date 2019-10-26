(load "constraint.scm")

(define (c+ x y)
  (define z (make-connector))
  (adder x y z)
  z)

(define (c* x y)
  (define z (make-connector))
  (multiplier x y z)
  z)

(define (c/ x y)
  (define z (make-connector))
  (multiplier z y x)
  z)

(define (cv v)
  (define z (make-connector))
  (constant v z)
  z)

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
(probe "Fahrenheit temp" F)
(set-value! F 212 'user)
(probe "Celsius temp" C)