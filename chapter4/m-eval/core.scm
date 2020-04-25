;(load "eval-analyze.scm")
;(load "eval-old.scm")

(define apply-in-underlying-scheme apply)

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (scan-out-defines body)
  (let ((defs (filter definition? body)))
   (if (null? defs)
       body
       (let* ((defs-vars (map definition-variable defs))
              (defs-vals (map definition-value defs))
              (unassigned-vars (map (lambda (var) (list var ''*unassigned*)) defs-vars))
              (assignments (map (lambda (var val) (list 'set! var val)) defs-vars defs-vals))
              (other-expressions (filter (lambda (e) (not (definition? e))) body)))
         (list (make-let unassigned-vars
                         (make-begin (append assignments other-expressions))))))))

; temporary disabled scan-out-defines as with old eval func
; body will be a sequence, but with analyzing it's a lambda
; and i don't want to adapt scan-out-defines to work with both
; flows simultaneusly
(define (make-procedure parameters body env)
  ;(list 'procedure parameters (scan-out-defines body) env))
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
         (scan (frame-variables frame)
               (frame-values frame)))))
  (let ((val (env-loop env)))
   (if (eq? val '*unassigned*)
       (error "Usage of unassigned variable" var)
       val)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
         (scan (frame-variables frame)
               (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
   (define (scan vars vals)
     (cond ((null? vars)
            (add-binding-to-frame! var val frame))
           ((eq? var (car vars)) (set-car! vals val))
           (else (scan (cdr vars) (cdr vals)))))
   (scan (frame-variables frame) (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '> >)
        (list '< <)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'disp display)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((null? exp) true)
        (else false)))

; VARIABLES
(define (variable? exp) (symbol? exp))

; QUOTATION LISTS
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; ASSIGNMENT
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; DEFINITION
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
'ok)
(define (make-definition var value) (list 'define var value))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

; LAMBDAS
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; AND, OR
(define (chain-check predicate? value null-value)
  (define (iter exp env)
    (cond ((null? exp) null-value)
          ((predicate? (eval (car exp) env)) value)
          (else (iter (cdr exp) env))))
  iter)

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (make-and seq) (cons 'and seq))
(define (make-or seq) (cons 'or seq))
(define eval-and (chain-check false? false true))
(define eval-or (chain-check true? true false))

; BEGIN AND SEQUENCE OF EXPRESSIONS
(define (make-begin seq) (cons 'begin seq))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

; LET
(define (make-let variables body) (list 'let variables body))
(define (make-named-let name variables body) (list 'let name variables body))
(define (let? expr) (tagged-list? expr 'let))
(define (named-let? expr) (symbol? (cadr expr)))
(define (let-name expr) (cadr expr))
(define (let-variables expr) (map car ((if (named-let? expr) caddr cadr) expr)))
(define (let-variables-values expr) (map cadr ((if (named-let? expr) caddr cadr) expr)))
(define (let-body expr) ((if (named-let? expr) cdddr cddr) expr))
(define (let->combination expr)
  (define (make-lambda-from-let expr)
    (make-lambda (let-variables expr) (let-body expr)))
  (let ((func (make-lambda-from-let expr))
        (var-values (let-variables-values expr)))
   (if (named-let? expr)
       (cons (make-lambda ()
                          (list (make-begin (list (make-definition (let-name expr) func)
                                                  (cons (let-name expr) var-values)))))
             ())
       (cons func var-values))))

(define (make-let* variables body) (list 'let* variables body))
(define (let*? expr) (tagged-list? expr 'let*))
(define (let*->nested-lets expr)
  (define (nest variable-list body)
    (if (null? variable-list)
        (make-let variable-list body)
        (make-let (list (car variable-list)) (nest (cdr variable-list) body))))
  (nest (cadr expr) (caddr expr)))

(define (letrec? expr) (tagged-list? expr 'letrec))
(define (letrec-inits expr) (cadr expr))
(define (letrec-body expr) (cddr expr))
(define (declare-variables expr)
  (map (lambda (x) (list (car x) '*unassigned*)) (letrec-inits expr)))
(define (set-variables expr)
  (map (lambda (x) (list 'set! (car x) (cadr x))) (letrec-inits expr)))
(define (letrec->let expr)
  (list 'let (declare-variables expr)
        (make-begin (append (set-variables expr) (letrec-body expr)))))

; FOR
; func prototype (for func start stop step)
; provided func must have 1 argument arity
; provided func should be used only for its side-effects
(define (for? exp) (tagged-list? exp 'for))
(define (for-func expr) (cadr expr))
(define (for-start expr) (caddr expr))
(define (for-stop expr) (cadddr expr))
(define (for-step expr) (cadddr (cdr expr)))
(define (for->expr expr)
  (let ((stop (for-stop expr))
        (step (for-step expr))
        (func (for-func expr))
        (check-func (if (< (for-start expr) (for-stop expr)) '< '>)))
    (make-named-let 'iter-func (list (list 'cur (for-start expr)))
              (make-if (list check-func 'cur stop)
                       (make-begin (list
                                     (list func 'cur)
                                     (list 'iter-func
                                           (list '+ 'cur step))))
                       'cur))))

; IF
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; COND
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-rocket-clause? clause) (and (= (length clause) 3) (eq? (cadr clause) '=>)))
(define (cond-rocket-test clause) (car clause))
(define (cond-rocket-recipient clause) (caddr clause))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause (let ((first (car clauses))
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF" clauses)))
              ((cond-rocket-clause? first)
               (make-if (cond-rocket-test first)
                        (list (cond-rocket-recipient first) (cond-rocket-test first))
                        (expand-clauses rest)))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))


; APPLICATION - any compound expression that is not defined explicitly
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
   (let ((output (eval input the-global-environment)))
    (announce-output output-prompt)
    (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

;(driver-loop)
