(load "procedures_as_arguments.scm")

(define (integrate-simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (multiplier index)
    (cond ((or (= index 0) (= index n)) 1.0)
	  ((even? index) 2.0)
	  (else 4.0)))
  (define (term index)
    (* (multiplier index)
       (f (+ a (* index h)))))
  (* h (sum term 0 inc n)))


(integrate-simpsons-rule cube 0 1 100)
(integrate-simpsons-rule cube 0 1 1000)
