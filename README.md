# Plume 🦚

## About
Plume aims to be a strongly, statically typed, and compiled languaged. It strives to capture the performance of C++ along with the ergonomics of newer languages such as Rust, Scala, and Go. It differs from Rust in the fact that it will require C/C++ esque memory management. 

This project is my first programming language.

### Motivation 
This project will to be a powerful learning experience that lends itself to constant improvement. Also, the codebase for this project will be useful for exploring other areas of PL later on, such as making an interpreter or experimenting with different runtime systems and garbage collection. Oh, and because it's fun and something I'll always be happy I did 🙂

### Usage
This project relies on [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to manage dependencies. Clone the repo and `cd` into it, then run `stack install`. You will now have the `plume` command on your `$PATH`, so run `plume --help` for your options. For now, you can run `plume compile -a [filename]` to print the AST for a Plume source file. If you wish to validate the semantics of a plume source file, you can run `plume compile -v [filename]`. This will perform type-checking and scope-resolution. You can run `plume compile --help` for more info. 

In the most recent Plume update, you can now run a program that consists only of a `main(): Int` function that immediately returns an exit code! To do this, run `plume run [filename]`. Check `plume run --help` for more options, of which there are currently none. Using `plume run [filename]` will run the given Plume program's bytecode using the Plume VM. 

### Approach
This compiler will be written in Haskell with a Parsec-based frontend and will target x86-64 assembly. It will also feature an intermediate bytecode representation that can be run in the Plume VM. This VM will be used to test against the behavior of the emitted assembly and it could possibly turn into its own project.

## Current State
The latest progress is the creation of a type-checking system and a system for validating scoping errors. The next step is to begin with the translation part of the compiler. Plume is in a "get this to work" state. I have forgone the creation of some basic programming constructs (i.e for-loops, custom types, system calls, etc) in order to accelerate progress toward getting a small subset of my language to compile and run successfully. Once that is achieved, the focus will be on expanding that subset. 

Plume currently only supports the following type of program (note: this is *extremely* temporary):

```
def main(): Int := 42
```

### Examples
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
