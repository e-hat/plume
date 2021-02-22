# Plume

# About
This project is my first programming language. It is currently in its very early stages. 

Plume aims to be a strongly, statically typed, and compiled languaged. It attempts to capture the performance of C++ along with the ergonomics of newer languages such as Rust, Scala, and Go. It differs from Rust in the fact that it will require C/C++ esque memory management. Above all else, this project also aims to be a powerful learning experience that lends itself to constant improvement.

## Approach
This compiler will be written in Haskell with a Parsec-based frontend and a backend built on LLVM.

## Current State
I am currently building the frontend using Parsec. I then plan to move on to translating the AST that is generated into LLVM IR. I am unfamiliar with both libraries so this will be a learning experience on many fronts.

## Examples
I will put an examples in this section when the language is capable of running small programs. However, below is what I'm aiming towards:

```
bring std::io;
bring std::lib;

def factorial(Int n): Int := 
  if n = 0 => 1
  else     => n * factorial(n - 1)

def main([String] args): Int := {
  n := 5;
  io::putLn(factorial(n));
  lib::STATUS_SUCCESS
}
```
