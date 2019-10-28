(define (get-signal wire) ())
(define (set-signal! wire new-value) ())
(define (add-action! wire action) ())

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal - LOGICAL-NOT" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay 
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and x y)
  (cond ((and (= x 0) (= y 0)) 0)
        ((and (= x 0) (= y 1)) 0)
        ((and (= x 1) (= y 0)) 0)
        ((and (= x 1) (= y 1)) 1)
        (else (error "Invalid signal -- LOGICAL-AND" x y))))

(define (or-gate a1 a2 output)
  (define a1i (make-wire))
  (define a2i (make-wire))
  (define s (make-wire))
  (and-gate a1 a2 s)
  (inverter a1 a1i)
  (inverter a2 a2i)
  (and-gate a1i a2i s)
  (inverter s output)
  'ok)