(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
   (let ((proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
              (if (eq? type1 type2)
                  (error "No method for these types" (list op type-tags))
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                            (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                            (apply-generic op a1 (t2->t1 a2)))
                          (else (error "No method for these types"
                                       (list op type-tags)))))))
            (error "No method for these types"
                   (list op type-tags)))))))

(define (apply-generic op . args)
  (define (coerce-types target-type source-types)
    (let ((target-types (map (lambda (source-type) target-type) source-types)))
     (let ((proc (get op target-types)))
      (if (and proc
               (every (lambda (source target) (or (eqv? source target) (get-coercion source target))) source-types target-types))
          (cons proc (map (lambda (source-type target-type arg)
                            (if (eqv? source-type target-type)
                                (contents arg)
                                ((get-coercion source-type target-type) (contents arg))))
                          source-types
                          target-types
                          args))
          #f))))
  (define (iter types-list source-types)
    (if (null? types-list)
        #f
        (let ((coerce-res (coerce-types (car types-list) source-types)))
         (if coerce-res
             coerce-res
             (iter (cdr types-list) source-types)))))
  (let ((type-tags (map type-tag args)))
   (let ((proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((coerce-pair (iter type-tags type-tags)))
         (if coerce-pair
             (apply (car coerce-pair) (cdr coerce-pair))
             (error "No method for these types" (list op type-tags))))))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

