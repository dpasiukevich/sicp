(data-paths
  (registers
    ((name a)
     (buttons ((name a<-b) (source (register b)))))
    ((name b)
     (buttons ((name b<-t) (source (register t)))))
    ((name t)
     (buttons ((name t<-r) (source (operation rem))))))
  (operations
    ((name rem) (inputs (register a) (register b)))
    ((name =) (inputs (register b) (constant 0)))))
(controller test-b
                (test (op =) (reg b) (const 0))
                (branch (label gcd-done))
                (assign t (reg a))
            rem-loop
                (test (op <) (reg t) (reg b))
                (branch (label rem-done))
                (assign t (op -) (reg t) (reg b))
                (goto (label rem-loop))
            rem-done
                (assign a (reg b))
                (assign b (reg t))
                (goto (label test-b))
            gcd-done)
