The high level architecture is illustrated as below:

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

Note that there is one design goal to keep in mind, _never introduce_
_any meaningless symbol unless real necessary_, we do optimizations,
however, it should still emit reasonable readable output code.