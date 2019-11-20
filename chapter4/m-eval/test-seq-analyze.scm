(define (gen-sequence size)
  (define (iter res n)
    (if (= n 0)
        res
        (iter (cons '(lambda () (disp 1)) res) (- n 1))))
  (iter () size))
(define (time-func func)
  (define (start-test start-time)
    (func)
    (- (runtime) start-time))
  (start-test (runtime)))


(define sequence (make-begin (gen-sequence 3)))

(define sequence (make-begin (gen-sequence 100000)))

(define x '((lambda () (define (loop n) (if (> n 0) (loop (- n 1)))) (loop 100000))))

(load "core.scm")
(load "eval-old.scm")

; takes 10s
(display (time-func (lambda () (eval x the-global-environment)))) (newline)

(load "core.scm")
(load "eval-analyze.scm")

; 5.7 s
(display (time-func (lambda () (eval x the-global-environment)))) (newline)

((lambda () (define x 3) (+ x 1)))

((lambda () (+ 3 2)))
