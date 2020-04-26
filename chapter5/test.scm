(load "assembler.scm")
(load "machine_model.scm")

(define test-machine
  (make-machine
    '(a b t)
    '()
    '()
    )
  )

(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b (test (op =) (reg b) (const 0))
             (branch (label gcd-done))
             (assign t (op rem) (reg a) (reg b))
             (assign a (reg b))
             (assign b (reg t))
             (goto (label test-b))
             gcd-done
             (save a)
             (restore a)
             )))
(machine-info gcd-machine)

(set-register-contents! gcd-machine 'a 126)
(set-register-contents! gcd-machine 'b 84)
(start gcd-machine)
(get-register-contents gcd-machine 'a) 

(define fib-machine
  (make-machine
    '(continue n val)
    (list (list '+ +) (list '- -) (list '< <))
    '(
      (assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      ;; set up to compute Fib(n   1)
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n) ; save old value of n
      (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
      (goto (label fib-loop)) ; perform recursive call
    afterfib-n-1 ; upon return, val contains Fib(n   1)
      (restore n)
      ;; set up to compute Fib(n   2)
      (assign n (op -) (reg n) (const 2))
      (assign continue (label afterfib-n-2))
      (save val) ; save Fib(n   1)
      (goto (label fib-loop))
    afterfib-n-2 ; upon return, val contains Fib(n   2)
      (assign n (reg val)) ; n now contains Fib(n   2)
      (restore val) ; val now contains Fib(n   1)
      (restore continue)
      (assign val ; Fib(n   1) + Fib(n   2)
      (op +) (reg val) (reg n))
      (goto (reg continue)) ; return to caller, answer is in val
    immediate-answer
      (assign val (reg n)) ; base case: Fib(n) = n
      (goto (reg continue))
    fib-done)))

(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(get-register-contents fib-machine 'val) 
