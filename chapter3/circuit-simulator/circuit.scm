(load "wires.scm")

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (logical-and a b) (* a b))
(define (logical-or a b) (if (or (= a 1) (= b 1)) 1 0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
     (after-delay inverter-delay
                  (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gat a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
     (after-delay or-gate-delay
                  (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
   (or-gate a b d)
   (and-gate a b c)
   (inverter c e)
   (and-gate d e s)
   'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
   (half-adder b c-in s c1)
   (half-adder a s sum c2)
   (or-gate c1 c2 c-out)
   'ok))

(define (n-bit-ripple-carry-adder A B S c-out)
  (let ((wires (map (lambda (x) (make-wire)) (cdr A))))
   (for-each full-adder
             A
             B
             (append wires (list (make-wire)))
             S
             (cons c-out wires))
   'ok))

