(load "assembler.scm")
(load "evaluator_machine.scm")

(load "compile_and_go.scm")

(define f '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))))
(assemble (statements (compile f 'val 'return)) eceval)

(pp (statements (compile f 'val 'return)))

(compile f 'val 'return)

(pp (compile f 'val 'return))

(statements (compile f 'val 'return))


(compile-and-go
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(compile-and-go '(if (= 5 2) 1 2))


(pp (compile '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))) 'val 'next))

(define c (compile '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))) 'val 'next))

c
