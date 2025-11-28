# me7aLang

**me7aLang** (metaLang) is a general-purpose low-level programming language created for educational purposes.  
The compiler is written in **C**, with a small number of dependencies and a simple design focused on clarity and learning.

## Features
- Written entirely in C
- Minimal dependencies
- Simple architecture for educational use
- Supports a growing subset of language features

## Getting Started
1. Clone the repository:
```bash
git clone https://github.com/me7alix/me7aLang.git
cd me7aLang
```

2. Build the compiler:
```bash
make
```

3. Compile and run an example:
```bash
./build/metc -o ./build/fib ./examples/fib.m7
./build/fib
```

## Examples
```
import "std.m7"

fn main() {
    printf("Hello, World!\n")
}
```
Check the [examples](./examples) directory to see what’s currently implemented.

## License

This project is released under the MIT License.
