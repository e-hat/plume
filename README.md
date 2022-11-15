# Plume 🦚

## About
Plume is a statically typed programming language that compiles to Web Assembly and ARM. I use this project to mess around with compilers stuff. Plume probably wouldn't be very useful in production.

Here's an overview of the syntax: https://www.eddiehatfield.com/blog/plume-syntax

## How things work
Check out `src/Parsing` for the abstract syntax tree, lexer, and parser, for which I 
used Haskell's Parsec library.

All of the semantic validation/type-checking takes place in `src/Semantics`, 
mainly in `Validation.hs`. This also builds up a symbol table that I use later on for the code generation phases.

Next, the AST is translated to a three-address code (TAC) IR, which you can see in 
`src/Ir/Tac/Translation.hs`.

Finally, this TAC IR is handed off to the backend. If compiling for WebAssembly,
the code is translated to an internal representation of WebAssembly copied from
[the official
spec](https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md).

Otherwise, if compiling for ARM, the TAC IR virtual registers get mapped to
physical registers or stack memory. This is called register allocation, or
RegAlloc in the codebase. At the moment, it just spills everything to the stack.

Then, this TAC gets emitted as textual ARM assembly.

Then, it gets linked against `libc` by compiling with `gcc` for ARM.

To compile an ARM binary, you'll need `arm-linux-gnueabihf-gcc` (or at least that's how I do it). Then, run the
following:
```bash
$ chmod u+x compile-arm.sh 
$ ./compile-arm.sh <plumefile>
```

If you have your own ARM linker setup you can use that instead.

## Roadmap

- [X] AST definition
- [X] Parsing 
- [X] Type-checking 
- [X] Translation to TAC
- [X] Translation from TAC to Wasm
- [X] Emit valid Wasm programs
- [X] Emit valid ARM programs
- [X] More stuff

More stuff:
- [X] Adding an ARM backend
    - [X] Naive register allocation (spill everything)
    - [X] CodeGen ARM assembly 
    - [ ] Smarter register allocation, based on linear-scan
- [ ] Control flow graphs from TAC
