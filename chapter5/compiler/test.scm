(load "assembler.scm")
(load "machine_model.scm")

(define test-machine
  (make-machine '() '()))

(define gcd-machine
  (make-machine
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

(machine-set-breakpoint gcd-machine 'test-b 3)

;(machine-cancel-breakpoint gcd-machine 'test-b 3)
(machine-cancel-all-breakpoints gcd-machine)

(start gcd-machine)

(set-register-contents! gcd-machine 'a 8)
(machine-proceed gcd-machine)

(get-register-contents gcd-machine 'a) 

(define fib-machine
  (make-machine
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
    fib-done
    (perform (op print-stack-statistics))
    )))
(machine-info fib-machine)

(set-register-contents! fib-machine 'n 5)
(trace-on fib-machine)

(machine-set-breakpoint fib-machine 'afterfib-n-1 4)

(machine-cancel-breakpoint fib-machine 'afterfib-n-1 4)

(start fib-machine)
(get-register-contents fib-machine 'val)
(print-instruction-count fib-machine)

(machine-proceed fib-machine)

(get-register-contents fib-machine 'val)
