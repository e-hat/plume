# Plume 🦚

## About
Plume is a statically typed programming language that compiles to Web Assembly. I use this project to mess around with compilers stuff. Plume probably wouldn't be very useful in production.

Here's an overview of the syntax: https://www.eddiehatfield.com/blog/plume-syntax

## How things work
Check out `src/Parsing` for the abstract syntax tree, lexer, and parser, for which I 
used Haskell's Parsec library.

All of the semantic validation/type-checking takes place in `src/Semantics`, 
mainly in `Validation.hs`. This also builds up a symbol table that I use later on for the code generation phases.

Next, the AST is translated to a three-address code (TAC) IR, which you can see in 
`src/Ir/Tac/Translation.hs`.

After this, the somewhat-linear TAC gets translated to a data type that represents Wasm in the Plume compiler. 
This data type is defined in `src/Wasm/Types.hs` and the translation from TAC to Wasm happens in `src/Wasm/Translation.hs`. 
Next, the wasm binary is emitted in `src/Wasm/Emit.hs`. 

## Roadmap

- [X] AST definition
- [X] Parsing 
- [X] Type-checking 
- [X] Translation to TAC
- [X] Translation from TAC to Wasm
- [X] Emit valid Wasm programs
- [ ] More stuff!

More stuff:
- [ ] Control flow graphs from TAC!

Working on control flow graphs atm!
