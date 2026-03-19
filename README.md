# me7aLang

**me7aLang** (metaLang) is a general-purpose low-level programming language created for educational purposes.
The compiler is written in **C**, with a small number of dependencies and a simple design focused on clarity and learning.

## Features
- Written entirely in C
- Minimal dependencies
- Simple architecture for educational use
- Supports a growing subset of language features

### Macro features
me7aLang supports object-like and function-like macros, like C, but with few extra features. Function-like macros are followed with brackets so it's much more convenient to write them.

You can pass blocks of code in brackets and it will be parsed as a single argument, so it's allowed to write such macros:
```
macro do_while(expr, body) {
    while true {
        block body

        if !(expr) {
            break
        }
    }
}
```

## Getting Started
1. Clone the repository:
```bash
git clone https://github.com/me7alix/me7aLang.git
cd me7aLang
```

2. Build the compiler:
```bash
make release -B
```

3. Compile and run an example:
```bash
./build/release/m7c -I ./stdlib -o ./build/fib ./examples/fib.m7
./build/fib
```

## Examples
```
import "std/libc.m7"

fn main() {
    printf("Hello, World!\n")
}
```
Check the [examples](./examples) directory to see what’s currently implemented.

## License

This project is released under the MIT License.

## Bugs
- **Pointer arithmetic** (on the level of TAC IR generating): ptr += 1 is not like ptr = ptr + 1
