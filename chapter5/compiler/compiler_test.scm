(load "compiler.scm")

(define c (compile
            '(define (factorial n)
               (if (= n 1)
                   1
                   (* (factorial (- n 1)) n)))
            'val
            'next))

(pp c)
