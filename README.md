### SICP exercises

#### Build and run
    docker build -t sicp . && docker run -it sicp

#### Usage
Main function is: `compile-and-go` with one argument - any valid scheme expression

Func will do:
1. Compile given expression down to machine code
2. Starts register-machine simulator. This is the machine for interpreting Scheme expressions
3. Compiled code from 1. is loaded and evaluated. 
4. REPL of register-machine is started.
5. You can pass any valid Scheme code for interpretation
6. Or you can use `(compile-and-run '(some exp))` to compile expression
7. Both 5. and 6. will be available in REPL for further use
8. ???
9. PROFIT


#### Example

```
1 ]=> (compile-and-go
  '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(total-pushes = 0 maximum-depth = 0)

;;EC-Eval value:
ok

;;EC-Eval input:
(factorial 5)

(total-pushes = 31 maximum-depth = 14)

;;EC-Eval value:
120

;;EC-Eval input:
(compile-and-run '(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))

(total-pushes = 0 maximum-depth = 0)

;;EC-Eval value:
ok

;;EC-Eval input:
(fib 5)

(total-pushes = 77 maximum-depth = 14)

;;EC-Eval value:
5

;;EC-Eval input:
(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(total-pushes = 3 maximum-depth = 3)

;;EC-Eval value:
ok

;;EC-Eval input:
(fib-iter 5)

(total-pushes = 222 maximum-depth = 10)

;;EC-Eval value:
5
```
