[LLVM Tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) Implemented in Rust

Compiler for the toy language Kaleidoscopic (modified)

```
# Kaleidoscope (modified) source code example

# import from math lib
extern sin(x)

# function definition
def fib(n)
    if n < 3 then
        1
    else
        fib(n-1) + fib(n-2)

# an expression to be evaluated, optional
find fib(10)
```

Features:

- JIT: evaluate the value of `find` expression at compile time
- Output LLVM IR (.ll and .bc files), which can be compiled to object files. Further, it can be linked with C language programs and interoperate
