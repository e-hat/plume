1. Use three-address code
2. Build cfg/basic-blocks from that
3. Generate WASM!
4. Done!!!


High-level steps to complete this plan:
1. Convert AST to three-address code
2. Scan thru three-address code (3AC) to form cfg
3. [this is where I would insert optimizations on the basic blocks when I do some]
4. Convert 3AC/cfg BACK to some kind of stack VR, probably tree structured?
5. Convert this to WASM and emit! (probably hard)


Step 1 - substeps
1. Strongly typed? - I would like this to possibly be checked at compile time
2. Create a data structure for 3AC
   - Needs an entire program type
       - mapping of string names to 3AC functions
       - function is tuple of (string -> Int) and [Instructions]
   - Needs the actual 3AC type. I would love the requirements in the following description to 
     be enforced by Haskell's type system.
      1. A = B $op$ C 
           - A cannot be a literal
           - B and C cannot be sub-expressions themselves - they must be literals or environment symbols
      2. A = $literal$
          - A cannot be a literal 
      3. if [$symbol$ | B $op$ C | $literal$] == false goto $label$
          - B and C cannot be sub-expressions
      4. goto $label$
3. Traverse AST to generate 3AC
    - Fold across the list of functions that represents a program
      1. initial state = empty program
      2. run a stateful action that converts AST node to a function
      3. add this function to the current program

Item 3.1.2
   - Initial state will be empty function generation state 
   - Need to keep track of variable names, label table, instruction #, and current list of instruction
   - Somewhat similar to original bytecode, although hopefully easier?

type Local = String
type InstResult = ([Inst], Local)
instsE :: ExprAug t -> TacGenerator InstResult
instsE (LitInt n) = do
  result <- nextLocal
  return ([Assignment (Literal n)], result)

Step 2 - "This shouldn't be too hard"
1. Create CFG data structure
