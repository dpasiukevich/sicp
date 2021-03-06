(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
(iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define r 1000)
(define (in-test-circle? x y)
  (<= (+ (square x)
         (square y))
      (square r)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test-experiment) 
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (let ((appr-square (* (monte-carlo trials test-experiment)
                        (* 1.0 (- x2 x1) (- y2 y1))))) ;rectangle square
    (/ appr-square (square r))))

(estimate-integral in-test-circle? (- r) r (- r) r 100000)
