<h1 align="center">Plume 🦚</h1>

<p align="center">Low-level programming language that's fun to write.</p>

### Table of Contents
* [ About ](#about)
  * [ Motivation ](#mot)
  * [ Approach ](#appr)
  * [ Current State ](#curr)

* [ Using Plume ](#use)
  * [ Installation ](#in)
  * [ How to write Plume ](#write)
  * [ How to run Plume ](#run)
  * [ Examples ](#examples)

<a name="about"></a>

## About
Plume aims to be a strongly, statically typed, and compiled languaged. It strives to capture the control + performance of C along with the ergonomics of newer languages such as Rust, Scala, and Go. It differs from Rust in the fact that it *will* require C/C++ esque memory management. 

This project is my first programming language.

<a name="mot"></a>

### Motivation 
I have a great time with expression-based syntax in functional programming languages like Scala and Haskell. I want to bring that same clarity of thought to the level of C, and while I love writing in C as well, it is in many ways tied to an older style of syntax. Plume is very similar to C, except that it can be optionally run in a VM and is more based around framing functions as expressions. 

I also wanted make a compiler, because who doesn't want their very own programming language?

<a name="appr"></a>

### Approach
This compiler will be written in Haskell with a Parsec-based frontend and will target x86-64 assembly. It will also feature an intermediate bytecode representation that can be run in the Plume VM. This VM will be used to test against the behavior of the emitted assembly and it could possibly turn into its own project.

<a name="curr"></a>

### Current State
The latest progress is the creation of a type-checking system and a system for validating scoping errors. The next step is to begin with the translation part of the compiler. Plume is in a "get this to work" state. I have forgone the creation of some basic programming constructs (i.e for-loops, custom types, system calls, etc) in order to accelerate progress toward getting a small subset of my language to compile and run successfully. Once that is achieved, the focus will be on expanding that subset. 

Plume currently only supports the following type of program (note: this is *extremely* temporary):

```
def main(): Int := 42
```

This program exits with a status of `42`. It exemplifies the motivation of the Plume syntax well -- `def main(): Int` is a function definition declaration that is assigned to the expression `42`, which is just an integer literal. 

<a name="use">

## Usage

<a name="in"></a>

### Installation

This project relies on [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to build and manage dependencies. Once you have that, do the following:
* Clone the repo and `cd` into it, then run `stack install`.
* This will install the `plume` executable to `~/.local/bin` if your computer is setup like mine. You can add that directory to your `$PATH` if you'd like.
* Run `plume --help`. If everything worked, you'll get some options to explore!

<a name="write"></a>

### How to write Plume programs

At some point I'll give an in depth discussion about the syntax rules of plume, but here's a simplified version. Statements in plume are either declarations or expressions.

A declaration is a `let` declaration, a reassignment, a function definition, a call declaration (function call followed by semicolon), an If/ElseIf/Else declaration, and a Block declaration. Declarations don't have outputs. You can't assign anything to them.

On the other hand, an expression is a variable substitution, a call expression (function call *not* followed by semicolon), a Block expression, an If/ElseIf/Else expression, or a math/boolean/relational expression. Also a `return` is a type of expression, mainly for ending control flow for functions that return `Void`. 

This example should clarify some things and show some of the common syntax:

```
# here's a function definition. These are only allowed in global scope.
# it is assigned to an expression, more specifically, a block expression
def func1(a: Int, b: Int): String := {
  # a block expression is a list of declarations followed by an expression
  # the first here is a let declaration
  Int fact := factorial(a)
  # here's another let declaration
  Bool hmm := a > b
  # and here's an if expression! note that each branch returns a result 
  # and each result is the same type (String)
  if hmm => "hmm was true!"
  else if not hmm => "hmm was false!"
  else => "this case will never run!"
}

# another function definition. A classic
def factorial(Int n): Int := 
  # just assign an if expression to your function!
  if n > 1 => n * factorial(n - 1)
  else => 1

# and here's the entry point
def main(String input): Int := {
  func1(5, 6);
  # this block expression just ends with an exit code
  5
}
```

Note that every plume program needs a `main` with that signature. I'll add some more examples, hopefully this told you a little about how Plume operates. If you're ever wondering about the specific rules, just check out `Syntax.hs`. Using Haskell means the syntax is practically written out in Backaus-Naur form.

<a name="run"></a>

### How to run Plume programs

Here's a brief overview of the available commands, but I would recommend looking at `plume --help` since I don't know if I want to keep updating this list. 

To run Plume programs, check out `plume run --help`. You pass it a Plume file, like `plume run plumefile.plm`, and your program will get run! This command uses the VM for Plume bytecode, so it won't be as fast as compiling to x86-64 and assembling then running the output.

To compile a Plume program to x86-64, along with some other options, check out `plume compile --help`. There's a couple options there that are pretty self-explanatory.

<a name="examples"></a>

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
