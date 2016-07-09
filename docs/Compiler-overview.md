## High Level compiler workflow

The high level architecture is illustrated below:

```
Source Language
  |
  | (Reuse OCaml Parser)
  v
Surface Syntax Tree
  |
  | (built in Syntax tree transformation)
  v
Surface Syntax Tree
  |
  | (Reuse OCaml Type checker)
  v
Typedtree
  |
  | (Reuse OCaml pattern match compiler and erase types)
  v
Lambda IR (OCaml compiler libs) ---+
  |   ^                            |                      
  |   |                     Lambda Passes (lam_* files) 
  |   |             Optimization/inlining/dead code elimination
  |   \                            |
  |    \ --------------------------+ 
  |
  |  Self tail call elimination
  |  Constant folding + propagation
  V
JS IR (J.ml)  ---------------------+
  |   ^                            |
  |   |                     JS Passes (js_* files)
  |   |            Optimization/inlining/dead code elimination
  |   \                            |
  |    \  -------------------------+
  |        
  |  Smart printer includes scope analysis 
  |
  V
Javascript Code 
```

## Design Principles

The current design of BuckleScript follows several high level principles. While those principles might change in the future, there are enforced today and can explain certain technical limitations BuckleScript has. 

**Lambda Representation** 

As pictured in the diagram above, BuckleScript is primarily based on the Lambda representation of the OCaml compiler. While this representation is quite rich, some information is lost from the upstream representation. The patch to the OCaml compiler tries to enrich this representation in a non-intrusive way (see next section). 

**Minimal Patch to the OCaml compiler**

BuckleScript requires patches to the OCaml compiler. One of the main reasons is to enrich the Lambda representation so that the generated code is as nice as possible. A design goal is to keep those patches minimal and useful for the OCaml compiler in general so that they can later be integrated. 

>A common question is to wonder why BuckleScript transpiles an OCaml record value to a JavaScript array while a more intuitive representation would be a JavaScript object. This technical decision is a direct consequence of the above 2 design principles: the Lambda layer assumes in a lot of places that a record value is an array and such modification would be too large of a change to OCaml compiler.

**Soundness**

BuckleScript preserves the soundness of the OCaml language. Assuming the FFI is correctly implemented, the type safety is preserved. 

**Minimal new symbol creation** 

In order to make the JavaScript generated code as close as possible to the original OCaml core we thrive to introduce as few new symbols as possible. 
