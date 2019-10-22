(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
	((not (pair? mobile)) mobile)
	(else (+ (total-weight (branch-structure (left-branch mobile)))
		 (total-weight (branch-structure (right-branch mobile))))))) 

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (is-mobile-balanced? mobile)
  (if (pair? mobile)
    (and (= (torque (left-branch mobile) (torque (right-branch mobile))))
	 (is-mobile-balanced? (branch-structure (left-branch mobile)))
	 (is-mobile-balanced? (branch-structure (right-branch mobile))))
    #t))
