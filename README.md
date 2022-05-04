# Plume 🦚

## About
This is a statically typed language that I've been writing a compiler for. A while 
ago it could emit a subset of x86-64 assembly but I am currently in the process 
of writing a WebAssembly backend instead, ~~which you can see on the `wasm` branch~~ which is being developed on the `main` branch. I'm tired of making feature branches when I'm the only one working on this lol.

You can get into the nitty-gritty details of the expresion-based syntax of Plume here: https://www.eddiehatfield.com/blog/plume-syntax

## How things work
Check out `src/Parsing` for the abstract syntax tree, lexer, and parser, for which I 
used Haskell's Parsec library.

All of the semantic validation/type-checking takes place in `src/Semantics`, 
mainly in `Validation.hs`. This also builds up a symbol table that I use later on for the code generation phases.

Next, the AST is translated to a three-address code (TAC) IR, which you can see in 
`src/Ir/Tac/Translation.hs`. I'll eventually make control-flow graphs from this and perform optimizations on those.

After this, the TAC is translated to Wasm, which happens in `src/Wasm/Translation.hs`. This is pretty simple. The Wasm representation is 
defined in `src/Wasm/Types.hs` and is copied pretty much directly from the offical Wasm docs, although a lot of that is missing because I simply don't need it for now. 

Once I've got my program represented in Wasm, I have to emit a Wasm binary. This is done in `src/Wasm/Emit.hs` and is also copied directly from the Wasm documentation. This is pretty straightforward. 

## Roadmap

- [X] AST definition
- [X] Parsing 
- [X] Type-checking 
- [X] Translation to TAC
- [X] Translation from TAC to Wasm
- [X] Emit valid Wasm programs
- [ ] More stuff!

Omg! I made it to the "More stuff!" item! I think my next goal is to have the TAC fit the requirements of Static Single Assignment (SSA). Then I can perform all sorts of optimizations on the result of that.
