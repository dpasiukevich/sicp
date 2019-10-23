(define (make-account balance pass)
  (define wrong-pass-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-cops) (error "GOTCHA M8"))
  (define (dispatch user-pass m)
    (if (not (eq? pass user-pass))
        (if (< wrong-pass-count 7)
            (begin (set! wrong-pass-count (+ wrong-pass-count 1)) (error "Incorrect password"))
            (call-cops))
        (cond ((eq? m 'withdraw) (set! wrong-pass-count 0) withdraw)
              ((eq? m 'deposit) (set! wrong-pass-count 0) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                    m)))))
  dispatch)

(define acc (make-account 100 'l))
((acc 'l 'withdraw) 105)
((acc 'l 'withdraw) 95)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)
((acc 'lul 'withdraw) 3)