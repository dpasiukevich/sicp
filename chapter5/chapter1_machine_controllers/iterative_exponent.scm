(controller
    (assign c (reg n))
    (assign p (const 1))
  exp-iter-loop
    (test (op =) (reg c) (const 0))
    (branch (label exp-done))
    (assign c (op -) (reg c) (const 1))
    (assign p (op *) (reg b) (reg p))
    (goto (label exp-iter-loop))
  exp-done
    (assign val (reg p)))
