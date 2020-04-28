(load "core.scm")
(load "eval-analyze.scm")

(load "evaluator_machine_code.scm")
(load "machine_model.scm")

(load "compiler.scm")

; helpers
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
    (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))
(define (get-global-environment) the-global-environment)  
(define eceval-operations 
  (list 
   ;;primitive Scheme operations 
   (list 'read read) 
   (list 'self-evaluating? self-evaluating?) 
   (list 'quoted? quoted?) 
   (list 'text-of-quotation text-of-quotation) 
   (list 'variable? variable?) 
   (list 'assignment? assignment?) 
   (list 'assignment-variable assignment-variable) 
   (list 'assignment-value assignment-value) 
   (list 'definition? definition?) 
   (list 'definition-variable definition-variable) 
   (list 'definition-value definition-value) 
   (list 'lambda? lambda?) 
   (list 'lambda-parameters lambda-parameters) 
   (list 'lambda-body lambda-body) 
   (list 'if? if?) 
   (list 'if-predicate if-predicate) 
   (list 'if-consequent if-consequent) 
   (list 'if-alternative if-alternative) 
   (list 'begin? begin?) 
   (list 'begin-actions begin-actions) 
   (list 'last-exp? last-exp?) 
   (list 'first-exp first-exp) 
   (list 'rest-exps rest-exps) 
   (list 'application? application?) 
   (list 'operator operator) 
   (list 'operands operands) 
   (list 'no-operands? no-operands?) 
   (list 'first-operand first-operand) 
   (list 'rest-operands rest-operands)
   (list 'cond? cond?) 
   (list 'cond->if cond->if)
   (list 'true? true?) 
   (list 'let? let?) 
   (list 'let->combination let->combination) 
   (list 'make-procedure make-procedure) 
   (list 'compound-procedure? compound-procedure?) 
   (list 'compiled-procedure? compiled-procedure?) 
   (list 'compiled-procedure-entry compiled-procedure-entry) 
   (list 'procedure-parameters procedure-parameters) 
   (list 'procedure-body procedure-body) 
   (list 'procedure-environment procedure-environment) 
   (list 'extend-environment extend-environment) 
   (list 'lookup-variable-value lookup-variable-value) 
   (list 'set-variable-value! set-variable-value!) 
   (list 'define-variable! define-variable!) 
   (list 'primitive-procedure? primitive-procedure?) 
   (list 'apply-primitive-procedure apply-primitive-procedure) 
   (list 'prompt-for-input prompt-for-input) 
   (list 'announce-output announce-output) 
   (list 'user-print user-print) 
   (list 'empty-arglist empty-arglist) 
   (list 'adjoin-arg adjoin-arg) 
   (list 'last-operand? last-operand?) 
   (list 'get-global-environment get-global-environment) 
   (list 'set-cdr! set-cdr!) 
   (list 'cdr cdr) 
   (list 'car car) 
   (list 'cons cons) 
   (list 'true? true?) 
   (list 'false? false?) 
   (list 'list list) 
   (list 'pretty-print pretty-print) 
   (list 'display display))) 
  
(define eceval 
  (make-machine 
    eceval-operations
    evaluator-machine-code))


(define the-global-environment (setup-environment))
;(start eceval)
