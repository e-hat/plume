# Plume 🦚

## About
This is a strongly typed language that I've been writing a compiler for. A while 
ago it could emit a subset of x86-64 assembly but I am currently in the process 
of writing a WebAssembly (Wasm) backend instead, which you can see on the `wasm` branch.

You can learn more about the expresion-based syntax of Plume here: https://www.eddiehatfield.com/blog/plume-syntax

## How things work
Check out `src/Parsing` for the abstract syntax tree, lexer, and parser, which I 
used the Parsec library to build.

All of the semantic validation/type-checking takes place in `src/Semantics`, 
mainly in `Validation.hs`.

Next, the AST is translated to a three-address code (TAC) IR, which you can see in 
`src/Ir/Tac`. I want to implement some optimizations here later on.

Finally, all Wasm related code is in, you guessed it, `src/Wasm`. 

## Roadmap

- [X] AST definition
- [X] Parsing 
- [X] Type-checking 
- [X] Translation to TAC
- [ ] Translation from TAC to Wasm
- [ ] Emit valid Wasm programs -- majorly in progress
- [ ] More stuff!
