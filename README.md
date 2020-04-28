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
5. ???
6. PROFIT

