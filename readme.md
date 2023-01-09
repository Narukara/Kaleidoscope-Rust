[LLVM Tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) Implemented in Rust

Compiler for the toy language Kaleidoscopic (modified)

```
# Kaleidoscope (modified) source code example

# import from math lib
extern sin(x)

# function definition
def foo(a, b)
    a*a - 2*a*b + b*b

def bar(x)
    (x+1) * (x-1)

# a expression to be evaluated, optional
find sin(foo(2,bar(1)))
```

Features:

- JIT: evaluate the value of `find` expression at compile time
- Output LLVM IR (.ll and .bc files), which can be compiled to object files. Further, it can be linked with C language programs and interoperate
