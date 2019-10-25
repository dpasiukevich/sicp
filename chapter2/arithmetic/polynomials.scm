(define (install-sparse-term-list-package) ; x**100 + 5*x**2 + 5
  (define (tag term-list) (attach-tag 'sparse-term-list term-list))
  (define (check-term-list term-list)
    (cond ((null? term-list) #t)
          ((list? term-list)
           (if (list? (car term-list))
               (check-term-list (cdr term-list))
               (error "element of list is not a list. should be list (order, coeff))")))
          (else (error "term-list must be a list, got: " term-list))))
  ; interface to rest of the system
  (put 'make 'sparse-term-list (lambda (term-list)
                                 (check-term-list term-list)
                                 (tag term-list)))
  (put 'first-term '(sparse-term-list) (lambda (term-list) (car term-list)))
  (put 'rest-terms '(sparse-term-list) (lambda (term-list) (tag (cdr term-list))))
  (put 'empty-termlist? '(sparse-term-list) null?)
  (put 'adjoin-term-func '(sparse-term-list)
       (lambda (term-list)
         (lambda (term) (tag (cons term term-list)))))
  'done)

(define (install-dense-term-list-package)
  (define (tag term-list) (attach-tag 'dense-term-list term-list))
  ; interface to rest of the system
  (put 'make 'dense-term-list (lambda (term-list)
                                (if (list? term-list)
                                    (tag (cons (- (length term-list) 1)
                                               term-list))
                                    (error "not a list, got: " term-list))))
  (put 'first-term '(dense-term-list) (lambda (term-list) (list (car term-list) (cadr term-list))))
  (put 'rest-terms '(dense-term-list) (lambda (term-list) (tag (cons (- (car term-list) 1) (cddr term-list)))))
  (put 'empty-termlist? '(dense-term-list) (lambda (term-list)
                                             (if (= (length term-list) 0)
                                                 (error "no index val in term-list")
                                                 (= (length term-list) 1))))
  (put 'adjoin-term-func '(dense-term-list)
       (lambda (term-list)
         (lambda (term)
           (let ((term-order (order term))
                 (head-order (car term-list))
                 (terms (cdr term-list)))
             (cond ((< term-order head-order)
                    (error "inserting in the middle of term list" term term-list))
                   ((= term-order head-order)
                    (tag (cons term-order
                               (cons (add (coeff term)
                                          (car terms))
                                     (cdr terms)))))
                   (else (adjoin-term term (tag (cons (+ 1 head-order)
                                                      (cons 0 terms))))))))))
  'done)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2) (and (variable? v1)
                                      (variable? v2)
                                      (eqv? v1 v2)))
  (define (check-polynoms-variables p1 p2) (same-variable (variable p1) (variable p2)))
  ;; representation of terms and term lists
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1) (negate-terms (term-list p2))))))
  (define (tag p) (attach-tag 'polynomial p))
  ;; interface to rest of the system
  (install-sparse-term-list-package)
  (install-dense-term-list-package)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) (lambda (p) (empty-termlist? (term-list p))))
  (put 'negate '(polynomial) (lambda (p) (tag (make-poly (variable p)
                                                         (negate-terms (term-list p))))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                   (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1)
                                 (rest-terms L2)))))))))

(define (negate-terms terms)
  (if (empty-termlist? terms)
      terms
      (let ((term (first-term terms)))
       (adjoin-term (make-term (order term)
                               (negate (coeff term)))
                    (negate-terms (rest-terms terms))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      L1
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      L
      (let ((t2 (first-term L)))
       (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))


(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      ((apply-generic 'adjoin-term-func term-list) term)))

(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list)
  (apply-generic 'empty-termlist? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

; dense polynomial has terms of most orders, represented as list of coefficients
; coef at index 0 has order L-1, last element has order 0
(define (make-dense-polynomial var terms)
  ((get 'make 'polynomial) var
                           ((get 'make 'dense-term-list) terms)))

; sparse polynomial don't have terms in most orders: x**100 + 5*x**2 + 5
; so it's represented as list of pairs (order coeff)
(define (make-sparse-polynomial var terms)
  ((get 'make 'polynomial) var
                           ((get 'make 'sparse-term-list) terms)))
