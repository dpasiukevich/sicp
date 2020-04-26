(load "assembler.scm")
(load "machine_model.scm")


(define fact-machine
  (make-machine
    (list (list '= =) (list '> >) (list '* *) (list '+ +))
    '(
      test-c
        (test (op >) (reg c) (reg n))
        (branch (label factorial-done))
        (assign p (op *) (reg c) (reg p))
        (assign c (op +) (reg c) (const 1))
        (goto (label test-c))
      factorial-done
        (perform (op print-stack-statistics))
      )))

(set-register-contents! fact-machine 'n 10)
(set-register-contents! fact-machine 'c 1)
(set-register-contents! fact-machine 'p 1)
(start fact-machine)
(get-register-contents fact-machine 'p)
