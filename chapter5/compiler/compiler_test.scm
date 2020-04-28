(load "compiler.scm")

(define c (compile
            '(define (factorial n)
               (if (= n 1)
                   1
                   (* n (factorial (- n 1)))))
            'val
            'next))

(define iter-c (compile 
                 '(define (factorial n)
                    (define (iter product counter)
                      (if (> counter n)
                          product
                          (iter (* counter product)
                                (+ counter 1))))
                    (iter 1 1))
                 'val
                 'next))

(pp c)

(pp iter-c)
