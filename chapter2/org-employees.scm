(load "set-tree.scm")
(load "set-ordered.scm")

(define t (make-equal-hash-table))
(define (put method division-id value)
  (hash-table-set! t (list method division-id) value))
(define (get method division-id)
  (hash-table-ref t (list method division-id)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (make-record division-id . args)
  (apply (get 'make-record division-id) args))

(define (get-record division-id employee-id)
  (attach-tag division-id ((get 'get-record division-id) employee-id)))

(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

; find record of employee in all divisions 
(define (find-employee-record employee-id divisions)
  (cond ((null? divisions) #f)
        (((get 'has-employee? (car divisions)) employee-id) (car divisions))
        (else (find-employee-record employee-id (cdr divisions))))) 

; id as salary Pog
; didn't want to update tree-set key from atomic value to first element of list
(define (register-division1)
  (define record-tree (make-tree 3000
                                 (make-tree 1500 () ())
                                 (make-tree 5000 () ()))) 
  (put 'make-record 'division1 (lambda (employee-id salary) (adjoin-set-tree (cons employee-id salary) record-tree)))
  (put 'has-employee? 'division1 (lambda (employee-id) (element-of-set-tree? employee-id record-tree)))
  (put 'get-record 'division1 (lambda (employee-id) (lookup-tree employee-id record-tree)))
  (put 'get-salary 'division1 cdr))

(define (register-division2)
  (define records (make-eqv-hash-table))
  (hash-table-set! records 101 2000)
  (hash-table-set! records 102 3000)
  (hash-table-set! records 103 4000)
  (put 'has-employee? 'division2 (lambda (employee-id) (hash-table/lookup records 
                                                                          employee-id
                                                                          (lambda (x) #t)
                                                                          (lambda () #f))))
  (put 'get-record 'division2 (lambda (employee-id) (hash-table-ref records employee-id)))
  (put 'get-salary 'division2 (lambda (x) x)))

(register-division1)

(register-division2)

(get-salary (get-record 'division1 1))

(get-salary (get-record 'division2 1034))

(find-employee-record 3000 '(division2 division1))
