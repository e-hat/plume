# Plume

## About
This project is my first programming language. It can currently parse a `.plm` file into an AST.

Plume aims to be a strongly, statically typed, and compiled languaged. It attempts to capture the performance of C++ along with the ergonomics of newer languages such as Rust, Scala, and Go. It differs from Rust in the fact that it will require C/C++ esque memory management. 

### Motivation 
This project aims to be a powerful learning experience that lends itself to constant improvement. Also, the codebase for this project will be useful for exploring other areas of PL later on, such as making an interpreter or experimenting with different runtime systems and garbage collection. Oh, and because it's fun and something I'll always be happy I did 🙂

## Usage
This project relies on [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to manage dependencies. Clone the repo and `cd` into it, then run `stack install`. You will now have the `plume` command on your `$PATH`, so run `plume --help` for your options. For now, you can run `plume -a [filename]` to print the AST for a Plume source file.

## Approach
This compiler will be written in Haskell with a Parsec-based frontend and will target WASM for easy cross-platform use (also because it's cool).

## Current State
While it is still missing some core syntax features, I have moved forward to building the symbol table on the semantics branch. This also involves checking for scoping errors. Next, I'll use the symbol table for type checking. Then, I move forward into the backend. However, I'm considering making an IR that is similar to C syntax to sort of "desugar" the syntactic irregularities of Plume.

## Examples
I will put an examples in this section when the language is capable of running small programs. However, below is what I'm aiming towards:

```
bring std::io;
bring std::lib;

def factorial(Int n): Int := 
  if n = 0 => 1
  else     => n * factorial(n - 1)

def main([String] args): Int := {
  Int n := 5;
  io::putLn(factorial(n));
  lib::STATUS_SUCCESS
}
```
