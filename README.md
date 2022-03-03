# Plume 🦚

## About
This is a strongly typed language that I've been writing a compiler for. A while 
ago it could emit a subset of x86-64 assembly but I am currently in the process 
of writing a WebAssembly backend instead, ~~which you can see on the `wasm` branch~~ which is being developed on the `main` branch. I'm tired of making feature branches when I'm the only one working on this lol.

You can get into the nitty-gritty details of the expresion-based syntax of Plume here: https://www.eddiehatfield.com/blog/plume-syntax

## How things work
Check out `src/Parsing` for the abstract syntax tree, lexer, and parser, for which I 
used Haskell's Parsec library.

All of the semantic validation/type-checking takes place in `src/Semantics`, 
mainly in `Validation.hs`. This also builds up a symbol table that I use later on for the code generation phases.

Next, the AST is translated to a three-address code (TAC) IR, which you can see in 
`src/Ir/Tac`. I'll eventually make control-flow graphs from this and perform optimizations on those.

Finally, all Wasm related code is in, you guessed it, `src/Wasm`. I currently have a data structure that I use 
to represent Wasm programs that is pretty much directly copied from the Wasm reference manual.

Right now, I'm working out translating TAC programs to Wasm programs. I'm going to make some changes to the way I do TAC that will make this (and many other things) easier.

## Roadmap

- [X] AST definition
- [X] Parsing 
- [X] Type-checking 
- [X] Translation to TAC
- [ ] Translation from TAC to Wasm
- [X] Emit valid Wasm programs
- [ ] More stuff!
