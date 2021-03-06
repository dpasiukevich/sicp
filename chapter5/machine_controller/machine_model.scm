(load "assembler.scm")

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
   ((machine 'install-operations) ops)
   ((machine 'install-instruction-sequence)
    (assemble controller-text machine))
   machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace? #f))
   (define (dispatch message)
     (cond ((eq? message 'get) contents)
           ((eq? message 'set)
            (lambda (value)
              (if trace?
                  (begin (display "set register name: ") (display name) (display " old_value: ") (display contents) (display " new_value: ") (display value) (newline)))
              (set! contents value)))
           ((eq? message 'trace-on) (set! trace? #t))
           ((eq? message 'trace-off) (set! trace? #f))
           (else
             (error "Unknown request: REGISTER" message))))
   dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))
(define (register-trace-on register) (register 'trace-on))
(define (register-trace-off register) (register 'trace-off))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
           (set! s (cdr s))
           (set! current-depth (- current-depth 1))
           top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0)
        (trace? #f))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
              (cons (list name (make-register name))
                    register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
         (if val
             (cadr val)
             (begin (allocate-register name) (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
         (if (null? insts)
             'done
             (let ((cur-inst (car insts)))
              (if (instruction-with-break? cur-inst)
                  (begin (display "break: ") (display (instruction-text cur-inst)) 'on-break)
                  (begin
                    (set! instruction-count (+ instruction-count 1))
                    (if trace? (begin (display (instruction-text (car insts)))
                                      (display " | labels: ")
                                      (display (instruction-preceding-labels (car insts))) (newline)))
                    ((instruction-execution-proc (car insts)))
                    (execute)))))))
      (define (proceed)
        (let ((insts (get-contents pc)))
         (if (null? insts)
             'done
             (begin (instruction-remove-break! (car insts)) (execute)))))
      (define (set-breakpoint label n)
        (instruction-set-break! (find-instr label n))
        'set-breakpoint)
      (define (cancel-breakpoint label n)
        (instruction-remove-break! (find-instr label n))
        'cancel-breakpoint)
      (define (cancel-all-breakpoints)
        (for-each (lambda (i) (instruction-remove-break! i)) the-instruction-sequence)
        'cancel-all-breakpoints)
      (define (find-instr label n)
        (define (iter instrs count)
          (cond ((null? instrs) (error "failed to find instr " label n))
                (else (let* ((cur-instr (car instrs))
                             (preceding-labels (instruction-preceding-labels cur-instr))
                             (same-label? (and (not (null? preceding-labels)) (eq? (car preceding-labels) label))))
                        (cond ((and same-label? (= count 1))
                               cur-instr)
                              (else (iter (cdr instrs) (if same-label? (- count 1) count))))))))
        (iter the-instruction-sequence n))
      (define (machine-info)
        (define (print-header header) (display header) (newline) (display "================") (newline))
        (define (print-list-newline l) (for-each (lambda (e) (display e) (newline)) l))
        (newline)
        (print-header "INSTRUCTIONS:")
        (print-list-newline (sort (delete-duplicates (map instruction-text the-instruction-sequence)) (lambda (i1 i2) (symbol<? (car i1) (car i2)))))
        (print-header "REGISTERS:")
        (display (sort (map car register-table) symbol<?)) (newline)
        (print-header "ENTRY REGISTERS:")
        (display (sort (delete-duplicates (map (lambda (inst) (register-exp-reg (goto-dest inst)))
                                               (filter (lambda (inst) (and (eq? (car inst) 'goto)
                                                                           (register-exp? (goto-dest inst))))
                                                       (map car the-instruction-sequence))))
                       symbol<?)) (newline)
        (print-header "STACK REGISTERS:")
        (display (sort (delete-duplicates (map (lambda (inst) (stack-inst-reg-name inst))
                                               (filter (lambda (inst) (member (car inst) '(save restore)))
                                                       (map car the-instruction-sequence))))
                       symbol<?)) (newline)
        (print-header "REGISTER SOURCES:")
        (let ((t (make-strong-eq-hash-table)))
         (for-each (lambda (inst)
                     (let ((reg (assign-reg-name inst)))
                      (hash-table-set! t reg (cons (assign-value-exp inst)
                                                   (hash-table-ref t reg (lambda () '()))))))
                   (delete-duplicates (filter (lambda (inst) (eq? (car inst) 'assign))
                                              (map car the-instruction-sequence))))
         (for-each (lambda (l) (display (car l)) (newline) (print-list-newline (cdr l))) (sort (hash-table->alist t) (lambda (e1 e2) (symbol<? (car e1) (car e2)))))
         )
        )
      (define (print-instruction-count) (display "instruction count: ") (display instruction-count) (newline) (set! instruction-count 0))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'machine-info) machine-info)
              ((eq? message 'print-instruction-count) print-instruction-count)
              ((eq? message 'trace-on) (lambda ()
                                         (set! trace? #t)
                                         ; (for-each (lambda (r) (display r) (register-trace-on r)) (map cadr register-table))
                                         'trace-on))
              ((eq? message 'trace-off) (lambda ()
                                          (set! trace? #f)
                                          ; (for-each (lambda (r) (register-trace-off r)) (map cadr register-table))
                                          'trace-off))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'proceed) proceed)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints) cancel-all-breakpoints)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (machine-info machine) ((machine 'machine-info)))

(define (print-instruction-count machine) ((machine 'print-instruction-count)))

(define (trace-on machine) ((machine 'trace-on)))
(define (trace-off machine) ((machine 'trace-off)))

(define (machine-set-breakpoint machine label n) ((machine 'set-breakpoint) label n))
(define (machine-proceed machine) ((machine 'proceed)))
(define (machine-cancel-breakpoint machine label n) ((machine 'cancel-breakpoint) label n))
(define (machine-cancel-all-breakpoints machine) ((machine 'cancel-all-breakpoints)))
