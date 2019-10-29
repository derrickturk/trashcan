/*!
# trashcan
*"The name of this trashcan is Visual Basic"*

**trashcan** is a simple language that compiles to Visual Basic (the old, gross one).

Work in progress, licensed for commercial or non-commercial use under GPL 3.0 or later.

*(C) 2017-2019 dwt | terminus data science, LLC*

## Features
* No more damn case insensitivity
* Short-circuiting boolean expressions
* Simplified iteration (for-each loops)
* Mandatory typing with limited type-checking enhancements
* Syntactic conveniences (e.g. += and friends)

### TODO
* Generic types and functions
* Array and structure literals
* Objects with constructors (use validated only for trashcan code)
* Somehow, improved error handling (?)
* Closures (?)

## Usage
Build the trashcan compiler using `cargo build`.

Compile trashcan source files (`.tc`) with `cargo run`:
```shell
$ cargo run -- input.tc
```
The trashcan compiler (`tcc`) will emit Visual Basic `.bas` files for each
module in your trash source code.

Run the tests using `cargo test`.
!*/


pub mod ast;
pub mod visit;
pub mod fold;
#[macro_use]
pub mod parser;
pub mod codegen;
#[macro_use]
pub mod analysis;
pub mod transform;
