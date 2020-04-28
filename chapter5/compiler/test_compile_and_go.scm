(load "assembler.scm")
(load "evaluator_machine.scm")
(assemble (statements (compile f 'val 'return)) eceval)

(define (compile-and-go expression)
  (let ((instructions
          (assemble
            (statements
              (compile expression 'val 'return))
            eceval)))
    (pp instructions)
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(define f '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))))

(compile f 'val 'return)

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
