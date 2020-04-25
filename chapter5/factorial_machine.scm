(data-paths
  (registers
    ((name p)
     (buttons ((name a<-b) (source (register b)))))
    ((name c)
     (buttons ((name b<-t) (source (register t)))))
    ((name n)
     (buttons ((name t<-r) (source (operation rem))))))
  (operations
    ((name rem) (inputs (register a) (register b)))
    ((name =) (inputs (register b) (constant 0)))))
(controller
  test-c
  (test (op >) (reg c) (reg n))
  (branch (label factorial-done))
  (assign p (op mul) (reg c) (reg p))
  (assign c (op add) (reg c) (const 1))
  (goto (label test-c))
  factorial-done)
