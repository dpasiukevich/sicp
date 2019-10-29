(define same-key? equal?)

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((not (pair? records)) #f)
          ((not (pair? (car records))) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (define (lookup keys local-table)
       (define (lookup-helper keys sub-table)
         (cond ((eq? sub-table #f) sub-table)
               ((null? keys) (cdr sub-table)) 
               (else (lookup-helper (cdr keys) (assoc (car keys) (cdr sub-table))))))
       (if (pair? keys) (lookup-helper keys local-table) #f))
  (define (insert! keys value local-table)
    (define (extend-table! table key new-value)
      (let ((table-elements (cdr table)))
       (set-cdr! table (cons (cons key new-value)
                             (if (pair? table-elements) table-elements ())))))
    (let ((sub-table (assoc (car keys) (cdr local-table))) ; sub-table is a pair (key, value
          (last-key? (null? (cdr keys))))
      (if last-key?
          (if sub-table
              (set-cdr! sub-table value)
              (extend-table! local-table (car keys) value))
          (if sub-table
              (insert! (cdr keys) value sub-table)
              (begin (extend-table! local-table (car keys) ())
                     (insert! (cdr keys) value (cadr local-table))
                     ))))
    value)
  (let ((t (list '*table*)))
   (define (dispatch m)
     (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys t)))
           ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys value t)))
           (else (error "Unknown operation: TABLE" m))))
   dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(x y) 15)
(put '(x) 150)
(get '(x y))
(get '(k))
(get '(y x))
(get '(x))
(put '(x y z) 22)
(get '(x y z))
(get '(x))
(get '(x y))
(get '(x y z k))
