(controller
    (assign continue (label exp-done)) ;set up final return address
  exp-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    ;; Set up for the recursive call by saving continue.
    ;; Set up continue so that the computation will continue
    ;; at after-exp when the subroutine returns.
    (save continue)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-exp))
    (goto (label exp-loop))
  after-exp
    (restore continue)
    (assign val (op *) (reg b) (reg val)) ;val now contains b * exp(b, n-1)
    (goto (reg continue)) ;return to caller
  base-case
    (assign val (const 1)) ;base case: x^0 = 1
    (goto (reg continue)) ;return to caller
  exp-done)
