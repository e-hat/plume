# Plume

## About
This project is my first programming language. It can currently parse a `.plm` file into an AST.

Plume aims to be a strongly, statically typed, and compiled languaged. It attempts to capture the performance of C++ along with the ergonomics of newer languages such as Rust, Scala, and Go. It differs from Rust in the fact that it will require C/C++ esque memory management. Above all else, this project also aims to be a powerful learning experience that lends itself to constant improvement. Also, the codebase for this project will be useful for exploring other areas of PL later on, such as making an interpreter or experimenting with different runtime systems and garbage collection. 

## Usage
This project relies on [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to manage dependencies. Clone the repo and `cd` into it, then run `stack install plume`. You will now have the `plume` command on your `$PATH`, so run `plume --help` for your options. For now, you can run `plume -a [filename]` to print the AST for a Plume source file.

## Approach
This compiler will be written in Haskell with a Parsec-based frontend and will target WASM for easy cross-platform use (also because it's cool).

## Current State
I am currently building the frontend using Parsec. My plan for this project is to GET THINGS WORKING then worry about doing more complex semantic analysis/runtime algorithms in the future.

I have setup parsing for boolean and arithmetic expressions and I am now implementing some core syntax of the language. Note that this is absolutely not a final definition of the syntax (e.g. it is missing for-loops). It is simply a starting place to get things working ASAP.

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
